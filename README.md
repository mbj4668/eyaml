# eyaml

eyaml is a NIF-based YAML 1.2 parser, based on the
[libyaml](https://pyyaml.org/wiki/LibYAML) "C" library.

- The NIF parser is re-entrant, which means that the NIF won't block
  the emulator.
- Handles the three YAML schemas `fail_safe`, `json`, and `core`.
- Expands aliases.
- Fast.

## Erlang representation

The different YAML tags are mapped to erlang types as follows:

YAML tag                  | Erlang type
--------------------------|----------------------------------
`tag:yaml.org,2002:map`   | `map() \| list({term(), term()})`
`tag:yaml.org,2002:seq`   | `list()`
`tag:yaml.org,2002:str`   | `binary()`
`tag:yaml.org,2002:null`  | `'null'`
`tag:yaml.org,2002:bool`  | `boolean()`
`tag:yaml.org,2002:int`   | `integer()`
`tag:yaml.org,2002:float` | `float() \| 'nan' \| 'inf' \| '-inf'`

A YAML mapping is by default represented as an Erlang map, unless the
option `mapping_as_list` is given, in which case the mapping is
represented as a key-value list.

In the `fail_safe` schema, all scalars are represented as binaries.

If the option `key_as_existing_atom` is given, mapping string keys are
converted to existing atoms.  If the atom doesn't exist, or if the
atom is one of `true`, `false`, `null`, `nan`, `inf` or `-inf`, then
the mapping string key is returned as a binary.

The `key_as_existing_atom` option is useful when the schema for the
YAML document is known, and all keys are already present in the code.

## API

See [eyaml.erl](src/eyaml.erl).

## Installation

### Dependencies

The minimum required Erlang/OTP version is 18.0

### Generic build

eyaml uses [erl.mk](https://github.com/mbj4668/erl.mk)

    make

## Example usage

```erlang
make shell
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> eyaml:parse(<<"foo: 42">>).
{ok,[#{<<"foo">> => 42}]}
```

## Option `key_as_existing_atom`

Converts mapping keys that are strings to existing atoms.

```erlang
2>  a,b,eyaml:parse(<<"a: 1\n'b': -3.0\nxyzu: 42">>, #{key_as_existing_atom => true}).
{ok,[#{a => 1,
       b => -3.0,
       <<"xyzu">> => 42}]}

3> eyaml:parse(<<"[1, 42: foo, a: b]: 42">>, #{key_as_existing_atom => true}).
{ok,[#{[1, #{42 => <<"foo">>}, #{a => <<"b">>}] => 42}]}

4> eyaml:parse(<<"true: .nan\n\"true\": \"null\"">>, #{key_as_existing_atom => true}).
{ok,[#{true => nan,
       <<"true">> => <<"null">>}]}

```

Parse a file with anchors and aliases.

```erlang
4> step,instrument,pulseDuration,repetition,pulseEnergy,spotSize.

5> eyaml:parse_file("test/test2.yaml", #{key_as_existing_atom => true}).
{ok,[[#{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 12,
              pulseEnergy => 5.4,
              repetition => 1000,
              spotSize => <<"1mm">>}},
      #{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 10,
              pulseEnergy => 5.0,
              repetition => 500,
              spotSize => <<"2mm">>}},
      #{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 12,
              pulseEnergy => 5.4,
              repetition => 1000,
              spotSize => <<"1mm">>}},
      #{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 10,
              pulseEnergy => 5.0,
              repetition => 500,
              spotSize => <<"2mm">>}},
      #{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 12,
              pulseEnergy => 5.4,
              repetition => 1000,
              spotSize => <<"1mm">>}},
      #{step =>
            #{instrument => <<"Lasik 2000">>,
              pulseDuration => 10,
              pulseEnergy => 5.0,
              repetition => 500,
              spotSize => <<"2mm">>}}]]}


```

## Option `mapping_as_list`

Convert YAML mappings into Erlang lists.

```erlang

6> eyaml:parse_file("test/test2.yaml",
   #{key_as_existing_atom => true, mapping_as_list => true}).
{ok,[[[{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.4},
              {pulseDuration,12},
              {repetition,1000},
              {spotSize,<<"1mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.0},
              {pulseDuration,10},
              {repetition,500},
              {spotSize,<<"2mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.4},
              {pulseDuration,12},
              {repetition,1000},
              {spotSize,<<"1mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.0},
              {pulseDuration,10},
              {repetition,500},
              {spotSize,<<"2mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.4},
              {pulseDuration,12},
              {repetition,1000},
              {spotSize,<<"1mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.0},
              {pulseDuration,10},
              {repetition,500},
              {spotSize,<<"2mm">>}]}]]]}
```

## Option `schema`

```erlang
7> eyaml:parse(<<"a: 42\nb: 1.0\nc: Null">>, #{schema => fail_safe}).
{ok,[#{<<"a">> => <<"42">>,
       <<"b">> => <<"1.0">>,
       <<"c">> => <<"Null">>}]}

8> eyaml:parse(<<"a: 42\nb: 1.0\nc: Null">>, #{schema => json}).
{ok,[#{<<"a">> => 42,
       <<"b">> => 1.0,
       <<"c">> => <<"Null">>}]}

9> eyaml:parse(<<"a: 42\nb: 1.0\nc: Null">>, #{schema => core}).
{ok,[#{<<"a">> => 42,
       <<"b">> => 1.0,
       <<"c">> => null}]}
```

## Option `max_nodes`

Limit the size of the parsed term.

An alias is expanded by sharing the anchored term, so a small document
can describe a term that is very large once it is traversed, printed
or copied:

```yaml
a: &a [x, x, x, x, x, x, x, x, x, x]
b: &b [*a, *a, *a, *a, *a, *a, *a, *a, *a, *a]
c: &c [*b, *b, *b, *b, *b, *b, *b, *b, *b, *b]
d: &d [*c, *c, *c, *c, *c, *c, *c, *c, *c, *c]
```

Each level is ten times larger than the previous one; ten such lines
give more than 10^10 nodes.

With `max_nodes`, the parser counts the nodes of the input and fails
with `{error, Line, too_many_nodes}` when there are more than the
given number.  A node is a scalar, a sequence or a mapping.  A
sequence counts as one node plus its entries, and a mapping as one
node plus its keys and values.  An alias counts as the term it refers
to, as if the term had been written out in its place.  The count
covers all documents in the input.

```erlang

10> eyaml:parse(<<"a: 1\nb: [x, y]\n">>, #{max_nodes => 7}).
{ok,[#{<<"a">> => 1,<<"b">> => [<<"x">>,<<"y">>]}]}
11> eyaml:parse(<<"a: &x [1, 2]\nb: *x\n">>, #{max_nodes => 8}).
{error,1,too_many_nodes}

```

The first document has 7 nodes: the mapping, the keys `a` and `b`, the
value `1`, and the sequence with its two entries.  The second has 9,
since the sequence of 3 nodes is counted both at `a` and at `b`.

An input without aliases has at most about one node per byte, so a
limit that is equal to the maximum size in bytes of the input the
application accepts never rejects such an input, and bounds the term
to what an input of that size could have described without aliases.
A lower limit can be used when the expected size of the documents is
known.

The default is `infinity`, which means that there is no limit.

## Development

### Run tests

    make eunit

    make dialyze
