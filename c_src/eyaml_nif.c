#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <math.h>

#include <erl_nif.h>
#include <yaml.h>

/*
 * A re-entrant YAML parser as a NIF.
 *
 * The string data to parse is passed from erlang to the NIF, and then
 * the erlang code calls `get_next_event()` repeatedly to get all
 * `libyaml` events as erlang terms.  The erlang code then builds the
 * resulting terms.
 *
 * `libyaml` either gets passed the complete string to parse, or
 * requires a callback function to fill in the data to parse.  If we
 * were to pass the complete string, we'd have to pass the complete
 * binary from erlang, and copy it to some internal buffer, and then
 * call `yaml_parser_parse()`.  But this requires an unnecessary copy.
 * So instead we pass the complete binary from erlang in every call to
 * `get_next_event()`, and keep track of where we are in the `offset` in
 * our state.
 */

struct eyaml_state {
    ErlNifPid owner; /* pid of the owning process */
    yaml_parser_t ps;
    int flags;
    int schema;
    ErlNifBinary input;
    size_t offset;
};

static ErlNifResourceType *eyaml_state_res_type = NULL;

#define SCHEMA_FAIL_SAFE 1
#define SCHEMA_JSON 2
#define SCHEMA_CORE 3

#define F_KEY_AS_EXISTING_ATOM (1 << 0)

static ERL_NIF_TERM am_sequence_start;
static ERL_NIF_TERM am_sequence_end;
static ERL_NIF_TERM am_mapping_start;
static ERL_NIF_TERM am_mapping_end;
static ERL_NIF_TERM am_document_end;
static ERL_NIF_TERM am_scalar;
static ERL_NIF_TERM am_alias;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_undefined;
static ERL_NIF_TERM am_null;
static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;
static ERL_NIF_TERM am_eof;
static ERL_NIF_TERM am_inf;
static ERL_NIF_TERM am_neg_inf;
static ERL_NIF_TERM am_nan;

static void
init_atoms(ErlNifEnv *env)
{
    am_sequence_start = enif_make_atom(env, "sequence_start");
    am_sequence_end = enif_make_atom(env, "sequence_end");
    am_mapping_start = enif_make_atom(env, "mapping_start");
    am_mapping_end = enif_make_atom(env, "mapping_end");
    am_document_end = enif_make_atom(env, "document_end");
    am_scalar = enif_make_atom(env, "scalar");
    am_alias = enif_make_atom(env, "alias");
    am_error = enif_make_atom(env, "error");
    am_undefined = enif_make_atom(env, "undefined");
    am_null = enif_make_atom(env, "null");
    am_true = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");
    am_eof = enif_make_atom(env, "eof");
    am_inf = enif_make_atom(env, "inf");
    am_neg_inf = enif_make_atom(env, "-inf");
    am_nan = enif_make_atom(env, "nan");
}

static void
destroy_eyaml_state(ErlNifEnv *env, void *obj)
{
    struct eyaml_state *es = obj;
    yaml_parser_delete(&es->ps);
    return;
}

static int
load(ErlNifEnv *env,
     void **priv,
     ERL_NIF_TERM load_info)
{
    init_atoms(env);
    eyaml_state_res_type =
        enif_open_resource_type(env, "eyaml", "state",
                                destroy_eyaml_state,
                                ERL_NIF_RT_CREATE, NULL);
    if (eyaml_state_res_type == NULL)
        return -1;

    return 0;
}

static int
upgrade(ErlNifEnv* env,
        void** priv_data,
        void** old_priv_data,
        ERL_NIF_TERM load_info)
{
    return 0;
}

/* Invoked by libyaml when more data is needed.
 * Fill from es->input, keep track of offet.
 */
static int
read_handler(void *data,
             unsigned char *buffer,
             size_t size,
             size_t *size_read)
{
    struct eyaml_state *es = data;

    if (es->offset >= es->input.size) {
        *size_read = 0;
        return 1;
    }
    if (size > es->input.size - es->offset) {
        size = es->input.size - es->offset;
    }
    memcpy(buffer, es->input.data + es->offset, size);
    es->offset += size;
    *size_read = size;
    return 1;
}

static ERL_NIF_TERM
init_nif(ErlNifEnv* env,
     int argc,
     const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    struct eyaml_state *es;
    int schema, flags;

    if (!enif_get_int(env, argv[0], &schema)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &flags)) {
        return enif_make_badarg(env);
    }

    es = enif_alloc_resource(eyaml_state_res_type, sizeof(*es));
    memset(es, 0, sizeof(*es));
    es->schema = schema;
    es->flags = flags;

    enif_self(env, &es->owner);

    yaml_parser_initialize(&es->ps);

    yaml_parser_set_input(&es->ps, read_handler, es);

    /* transfer ownership to calling process */
    ret = enif_make_resource(env, es);
    enif_release_resource(es);

    return ret;
}

static inline int
get_eyaml_state(ErlNifEnv *env,
                ERL_NIF_TERM arg,
                struct eyaml_state **es)
{
    ErlNifPid pid;

    if (!enif_get_resource(env, arg, eyaml_state_res_type, (void **)es)) {
        return 0;
    }
    memset(&pid, 0, sizeof(pid));
    enif_self(env, &pid);
    /* Ensure that the resource is only used by one (owning) process
       (easier than adding mutex around every call). */
    if (enif_compare_pids(&pid, &(*es)->owner) != 0) {
        return 0;
    }
    return 1;
}

static ERL_NIF_TERM
mk_binary(ErlNifEnv *env,
          const char *str)
{
    ERL_NIF_TERM eb;
    size_t sz = strlen(str);
    unsigned char *data = enif_make_new_binary(env, sz, &eb);

    memcpy(data, str, sz);
    return eb;
}

static ERL_NIF_TERM
mk_binary_size(ErlNifEnv *env,
               const char *str,
               size_t sz)
{
    ERL_NIF_TERM eb;
    unsigned char *data = enif_make_new_binary(env, sz, &eb);

    memcpy(data, str, sz);
    return eb;
}

static ERL_NIF_TERM
mk_anchor(ErlNifEnv *env,
          const unsigned char *str)
{
    if (str) {
        return mk_binary(env, (const char *)str);
    } else {
        return am_undefined;
    }
}

/*
 * YAML is rather specific about how numbers are represented.
 * Specifically, it's representation is not compatible with strtol(),
 * so we need additional code to capture these cases.
 *
 * NOTE: we don't handle bignums.
 */
static int
mk_number(ErlNifEnv *env,
          char *value,
          size_t length,
          int schema,
          ERL_NIF_TERM *ret)
{
    char *startptr, *endptr;
    long int i;
    double d;
    int base = 10;
    char buf[BUFSIZ];

    if ((length >= 2 && value[1] == 'X')
        || (length >= 3 && value[2] == 'X')) {
        /* YAML supports only 'x' for hexadecimal values,
           but strtol handles 'X'.  So this can never be a valid
           YAML number */
        return 0;
    }
    if (schema == SCHEMA_JSON && value[0] == '+') {
        /* JSON doesn't support numbers starting with '+' */
        return 0;
    }
    /* FIXME: JSON doesn't support leading 0s; need to check that */

    startptr = value;

    if ((length >= 2 && value[1] == 'x')
        || (length >= 3 && value[2] == 'x')) {
        /* this may be a hex number; strtol will deal with it */
        base = 16;
    } else if (length >= 2 && value[0] == '0' && value[1] == 'o') {
        base = 8;
        startptr = value + 2;
    } else if (length >= 3 && length < BUFSIZ
               && (value[0] == '+' || value[0] == '-')
               && value[1] == '0' && value[2] == 'o') {
        buf[0] = value[0];
        strncpy(buf+1, value+3, length-3);
        buf[length-2] = '\0';
        base = 8;
        startptr = buf;
    }

    if (schema == SCHEMA_JSON && (base == 8 || base == 16)) {
        return 0;
    }

    /* first try to make an integer */

    errno = 0;
    i = strtol(startptr, &endptr, base);
    if (*endptr == '\0'
        && !((errno == ERANGE && (i == LONG_MAX || i == LONG_MIN))
             || (errno != 0 && i == 0))) {
        *ret = enif_make_long(env, i);
        if (*ret) {
            return 1;
        }
    }

    /* then try to make a double (cannot be in other base 10) */

    if (base != 10) {
        return 0;
    }

    errno = 0;
    d = strtod(startptr, &endptr);
    if (*endptr == '\0'
        && isfinite(d)
        && !((errno == ERANGE && (d == HUGE_VAL || d == -HUGE_VAL))
             || (errno != 0 && d == 0))) {
        *ret = enif_make_double(env, d);
        if (*ret) {
            return 1;
        }
    }

    return 0;
}

static ERL_NIF_TERM
mk_scalar(ErlNifEnv *env,
          yaml_event_t *event,
          struct eyaml_state *es,
          int is_map_key)
{
    ERL_NIF_TERM ret;
    yaml_scalar_style_t style = event->data.scalar.style;
    char *value = (char *)event->data.scalar.value;
    size_t length = event->data.scalar.length;

    /* Here we handle int, floats, booleans and plain strings.  Which
       types we handle depend on the schema. */

    if (es->schema != SCHEMA_FAIL_SAFE && style == YAML_PLAIN_SCALAR_STYLE) {
        if (length == 4
            && (strncmp(value, "true", length) == 0
                || (es->schema == SCHEMA_CORE
                    && (strncmp(value, "True", length) == 0
                        || strncmp(value, "TRUE", length) == 0)))) {
            return am_true;
        } else if (length == 5
                   && (strncmp(value, "false", length) == 0
                       || (es->schema == SCHEMA_CORE
                           && (strncmp(value, "False", length) == 0
                               || strncmp(value, "FALSE", length) == 0)))) {
            return am_false;
        } else if ((length == 4 && strncmp(value, "null", length) == 0)
                   || (es->schema == SCHEMA_CORE
                       && (length == 0
                           || (length == 1 && strncmp(value, "~", length) == 0)
                           || (length == 4 &&
                               (strncmp(value, "Null", length) == 0
                                || strncmp(value, "NULL", length) == 0))))) {
            return am_null;
        } else if ((length == 4
                    && (strncmp(value, ".inf", length) == 0
                        || (es->schema == SCHEMA_CORE &&
                            (strncmp(value, ".Inf", length) == 0
                             || strncmp(value, ".INF", length) == 0))))
                   || (es->schema == SCHEMA_CORE && length == 5
                       && (strncmp(value, "+.inf", length) == 0
                           || strncmp(value, "+.Inf", length) == 0
                           || strncmp(value, "+.INF", length) == 0))) {
            return am_inf;
        } else if (length == 5
                   && (strncmp(value, "-.inf", length) == 0
                       || (es->schema == SCHEMA_CORE &&
                           (strncmp(value, "-.Inf", length) == 0
                            || strncmp(value, "-.INF", length) == 0)))) {
            return am_neg_inf;
        } else if (length == 4
                   && (strncmp(value, ".nan", length) == 0
                       || (es->schema == SCHEMA_CORE &&
                           (strncmp(value, ".NaN", length) == 0
                            || strncmp(value, ".NAN", length) == 0)))) {
            return am_nan;
        } else if (length > 0) {
            /* check if it is an integer */
            if (mk_number(env, value, length, es->schema, &ret)) {
                return ret;
            }
        }
    }
    if ((F_KEY_AS_EXISTING_ATOM & es->flags) && is_map_key) {
        if (enif_make_existing_atom_len(env, value, length,
                                        &ret, ERL_NIF_LATIN1)) {
            return ret;
        }
    }
    return mk_binary_size(env, value, length);
}

static ERL_NIF_TERM
mk_error(ErlNifEnv *env,
         yaml_parser_t *parser)
{
    return enif_make_tuple3(
        env,
        am_error,
        enif_make_uint(env, parser->problem_mark.line),
        mk_binary(env, parser->problem));
}

static ERL_NIF_TERM
get_next_event_nif(ErlNifEnv* env,
                   int argc,
                   const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM anchor, scalar;
    struct eyaml_state *es = NULL;
    yaml_event_t event;
    int res;
    int is_map_key;

    if (!get_eyaml_state(env, argv[0], &es)) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[1], &es->input)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[2], &is_map_key)) {
        return enif_make_badarg(env);
    }

    res = yaml_parser_parse(&es->ps, &event);
    if (!res) {
        return mk_error(env, &es->ps);
    }
    switch (event.type) {
    case YAML_STREAM_END_EVENT:
        return am_eof;
    case YAML_SEQUENCE_START_EVENT:
        anchor = mk_anchor(env, event.data.sequence_start.anchor);
        return enif_make_tuple2(env, am_sequence_start, anchor);
    case YAML_SEQUENCE_END_EVENT:
        return am_sequence_end;
    case YAML_MAPPING_START_EVENT:
        anchor = mk_anchor(env, event.data.mapping_start.anchor);
        return enif_make_tuple2(env, am_mapping_start, anchor);
    case YAML_MAPPING_END_EVENT:
        return am_mapping_end;
    case YAML_SCALAR_EVENT:
        anchor = mk_anchor(env, event.data.scalar.anchor);
        scalar = mk_scalar(env, &event, es, is_map_key);
        return enif_make_tuple3(env, am_scalar, anchor, scalar);
    case YAML_ALIAS_EVENT:
        anchor = mk_anchor(env, event.data.alias.anchor);
        return enif_make_tuple3(env, am_alias, anchor,
                                enif_make_uint(env, event.start_mark.line));
    case YAML_NO_EVENT:
    case YAML_STREAM_START_EVENT:
    case YAML_DOCUMENT_START_EVENT:
    case YAML_DOCUMENT_END_EVENT:
        /* we don't care about these */
        return get_next_event_nif(env, argc, argv);
    }
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
{
    {"init", 2, init_nif},
    {"get_next_event", 3, get_next_event_nif}
};

ERL_NIF_INIT(eyaml, nif_funcs, load, NULL, upgrade, NULL);
