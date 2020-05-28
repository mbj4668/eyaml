-module(eyaml).

-export([parse/1, parse/2,
         parse_file/1, parse_file/2]).

-export_type([eyaml/0, eyaml_seq/0, eyaml_map/0, eyaml_scalar/0]).

-on_load(load_nif/0).

-define(SCHEMA_FAIL_SAFE, 1).
-define(SCHEMA_JSON, 2).
-define(SCHEMA_CORE, 3).

-define(F_KEY_AS_ATOM, 1 bsl 0).
-define(F_MAPPING_AS_LIST, 1 bsl 1).

-define(bit_is_set(Fs, F), ((Fs) band (F) =/= 0)). % any bit in F is set in Fs

-type eyaml() :: eyaml_seq() | eyaml_map() | eyaml_scalar().

-type eyaml_seq() :: [eyaml()].

%% atom() below is only returned if parsing with option `key_as_atom`.
%% KVL below is only returned if parsing with option `mapping_as_list`.
-type eyaml_map() :: #{eyaml() | atom() => eyaml()}
                   | [{eyaml() | atom(), eyaml()}].

-type eyaml_scalar() :: fail_safe_scalar()
                      | json_scalar()
                      | core_scalar().

-type fail_safe_scalar() :: binary().

-type json_scalar() :: fail_safe_scalar()
                     | 'null' | integer() | boolean()
                     | float() | 'inf' | '-inf' | 'nan'.

-type core_scalar() :: json_scalar().

%% 'key_as_atom' means that strings in mappings keys are converted
%%     to existing atoms, otherwise they are binaries.
%%     The idea is that this is a useful option when the schema for
%%     the YAML document is known, and all keys are already present in
%%     the code.
%%     Note that when this option is used, mappings with string keys
%%     "null", "inf", "-inf", "nan", "true", and "false" will be
%%     converted to atoms, even if they are quoted strings in the YAML
%%     input.
%%
%% 'mapping_as_list' means that YAML mappings are converted into
%%     key-value lists.  The list entries order in the input is
%%     preserved.
%%     Note that when this option is used, the parser doesn't guarantee
%%     that the mapping does not contain duplicate keys.
%%
%% 'schema' specifies how scalar values are handled. The parser
%%     supports the recommended schemas in YAML 1.2.  The default
%%     schema is 'core'.
%%         'fail_safe' means that all scalars are treated as strings,
%%             and thus returned as binaries (except mapping keys, if
%%             'key_as_atom' is set).
%%         'json' also supports integers, floats, booleans and null.
%%         'core' supports the same types a 'json', but allows more
%%             lexical variation in the tokens (e.g., both "true" and
%%             "True" are accepted, and integers can be written in
%%             hex or octal forms).
-type parse_opts() :: #{'key_as_atom' => boolean(),
                        'mapping_as_list' => boolean(),
                        'schema' => 'fail_safe' | 'json' | 'core'}.

-type parse_ret() ::
        {ok, Documents :: [eyaml()]}
      | {error, Line :: integer(), {no_such_anchor, Anchor :: binary()}
                                 | binary()}.

-spec parse(Str :: iodata()) -> parse_ret().
parse(Str) ->
    parse(Str, #{}).

-spec parse(Str :: iodata(), parse_opts()) -> parse_ret().
parse(Str, Opts) ->
    Bin =
        if is_binary(Str) ->
                Str;
           is_list(Str) ->
                list_to_binary(Str)
        end,
    {Schema, Flags} = parse_opts(Opts),
    Res = init(Schema, Flags),
    try
        {ok, parse_loop({Res, Bin, Flags, new_anchors()}, [], false)}
    catch
        throw:Error ->
            Error
    end.

parse_file(FName) ->
    parse_file(FName, #{}).

parse_file(FName, Opts) when is_map(Opts) ->
    case file:read_file(FName) of
        {ok, Bin} ->
            parse(Bin, Opts);
        Error ->
            Error
    end.

%%% Internal functions

parse_loop(S, Acc, IsMapKey) ->
    {Res, Bin, Flags, Anchors} = S,
    Event = get_next_event(Res, Bin, int_from_bool(IsMapKey)),
    case Event of
        {sequence_start, Anchor} ->
            Term = parse_loop(S, [], false),
            add_anchor(Anchor, Term, Anchors),
            parse_loop(S, [Term | Acc], not IsMapKey);
        sequence_end ->
            lists:reverse(Acc);
        {mapping_start, Anchor} ->
            Term = parse_loop(S, [], true),
            add_anchor(Anchor, Term, Anchors),
            parse_loop(S, [Term | Acc], not IsMapKey);
        mapping_end ->
            mk_map(Acc, Flags);
        {scalar, Anchor, Term} ->
            add_anchor(Anchor, Term, Anchors),
            parse_loop(S, [Term | Acc], not IsMapKey);
        {alias, Anchor, Line} ->
            Term = get_anchor(Anchor, Line, Anchors),
            parse_loop(S, [Term | Acc], not IsMapKey);
        eof ->
            lists:reverse(Acc);
        Error ->
            throw(Error)
    end.

new_anchors() ->
    ets:new(eyaml_anchors, []).

add_anchor(undefined, _Term, _Anchors) ->
    ok;
add_anchor(Anchor, Term, Anchors) ->
    ets:insert(Anchors, {Anchor, Term}).

get_anchor(Anchor, Line, Anchors) ->
    case ets:lookup(Anchors, Anchor) of
        [{_, Term}] ->
            Term;
        [] ->
            throw({error, Line, {no_such_anchor, Anchor}})
    end.

parse_opts(Opts) ->
    Schema =
        case maps:get(schema, Opts, core) of
            fail_safe ->
                ?SCHEMA_FAIL_SAFE;
            json ->
                ?SCHEMA_JSON;
            _ ->
                ?SCHEMA_CORE
        end,
    Flags =
        case maps:get(key_as_atom, Opts, false) of
            true ->
                ?F_KEY_AS_ATOM;
            false ->
                0
        end
        bor
        case maps:get(mapping_as_list, Opts, false) of
            true ->
                ?F_MAPPING_AS_LIST;
            false ->
                0
        end,
    {Schema, Flags}.

int_from_bool(true) -> 1;
int_from_bool(false) -> 0.

mk_map(L, Flags) ->
    KVL = mk_map_list(L),
    if ?bit_is_set(?F_MAPPING_AS_LIST, Flags) ->
            %% `reverse` is strictly speaking not necessary,
            %% since mappings don't have an order
            lists:reverse(KVL);
       true ->
            maps:from_list(KVL)
    end.

%% NOTE: the list is reversed
mk_map_list([V, K | T]) ->
    [{K, V} | mk_map_list(T)];
mk_map_list([]) ->
    [].

%%% NIF handling

load_nif() ->
    SOPath = filename:join([code:priv_dir(eyaml), "eyaml_nif"]),
    erlang:load_nif(SOPath, 0).

init(_Schema, _Flags) ->
    nif_only().

get_next_event(_State, _Bin, _IsMapKey) ->
    nif_only().

nif_only() ->
    erlang:nif_error(eyaml_nif_not_loaded).

