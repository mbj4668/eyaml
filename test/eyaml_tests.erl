-module(eyaml_tests).

-include_lib("eunit/include/eunit.hrl").

test_file_path(File) ->
    F1 = filename:join(["test", File]),
    case filelib:is_file(F1) of
        true -> F1;
        _ -> filename:join(["..", "test", File])
    end.

temp_file_path() ->
    case filelib:is_dir("test") of
        true ->
            filename:join(["test", "temp_test.yaml"]);
        _ ->
            filename:join(["..", "test", "temp_test.yaml"])
    end.

parse_test1_test() ->
    FileName = test_file_path("test1.yaml"),
    ?assertEqual(
       {ok,[[{<<"Time">>,<<"2001-11-23 15:01:42 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Warning">>,
              <<"This is an error message for the log file">>}],
            [{<<"Time">>,<<"2001-11-23 15:02:31 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Warning">>,<<"A slightly different error message.">>}],
            [{<<"Date">>,<<"2001-11-23 15:03:17 -5">>},
             {<<"User">>,<<"ed">>},
             {<<"Fatal">>,<<"Unknown variable \"bar\"">>},
             {<<"Stack">>,
              [[{<<"file">>,<<"TopClass.py">>},
                {<<"line">>,23},
                {<<"code">>,<<"x = MoreObject(\"345\\n\")\n">>}],
               [{<<"file">>,<<"MoreClass.py">>},
                {<<"line">>,58},
                {<<"code">>,<<"foo = bar">>}]]}]]},
       eyaml:parse_file(FileName, #{mapping_as_list => true})).

parse_test2_test() ->
    FileName = test_file_path("test2.yaml"),
    ?assertEqual(
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
                     {spotSize,<<"2mm">>}]}]]]},
       eyaml:parse_file(FileName, #{key_as_existing_atom => true,
                                    mapping_as_list => true})).

parse_test3_test() ->
    FileName = test_file_path("test3.yaml"),
    ?assertEqual(
       {ok,[[{<<"a">>,123},
             {<<"b">>,<<"123">>},
             {<<"c">>,123.0},
             {<<"d">>,123},
             {<<"e">>,123},
             {<<"f">>,<<"Yes">>},
             {<<"g">>,<<"Yes">>},
             {<<"h">>,<<"Yes we have No bananas">>}]]},
       eyaml:parse_file(FileName, #{mapping_as_list => true})).

parse_test4_test() ->
    FileName = test_file_path("test4.yaml"),
    ?assertEqual(
       {ok,[[{<<"picture">>,
              <<"R0lGODlhDAAMAIQAAP//9/X\n17unp5WZmZgAAAOfn515eXv\n"
                "Pz7Y6OjuDg4J+fn5OTk6enp\n56enmleECcgggoBADs=mZmE\n">>}]]},
       eyaml:parse_file(FileName, #{mapping_as_list => true})).

parse_test5_test() ->
    FileName = test_file_path("test5.yaml"),
    ?assertEqual(
        {ok,[[
            {<<"Name">>,<<"Backslash">>},
                {<<"Source">>,<<"\\\\\\\\">>}],
            [{<<"Name">>,<<"Double_Quote">>},
                {<<"Source">>,<<"\"\"">>}],
            [{<<"Name">>,<<"Backslash_and_Double_Quote">>},
                {<<"Source">>,<<"\"\\\"\"">>}],
            [{<<"Name">>,<<"New_Line">>},
                {<<"Source">>,<<"\\n">>}]]
        },
       eyaml:parse_file(FileName, #{mapping_as_list => true})).


parse_test6_test() ->
    FileName = test_file_path("test6.yaml"),
    ?assertEqual(
       {ok,[[{<<"ints">>, [1, 2, 3]},
             {<<"value">>, true}
            ],
            [{true, true},
             {false, false},
             {<<"str">>, <<"123">>},
             {<<"str2">>, <<"123">>},
             {<<"int">>, 123},
             {null, null},
             {<<"null2">>, null},
             {<<"null3">>, null}
            ],
            [{<<"inbox">>,
              [
               {<<"enabled">>, true},
               {<<"filters">>,
                [[{<<"icon">>, <<"inbox">>}, {<<"label">>, <<"Inbox">>}]]}
              ]}]
           ]
       },
       eyaml:parse_file(FileName, #{mapping_as_list => true})).

parse_test7_test() ->
    FileName = test_file_path("test7.yaml"),
    ?assertEqual(
       {ok,[#{<<"foo">> =>
                  #{true => true,
                    false => false,
                    <<"false">> => false,
                    <<"str">> => <<"123">>,
                    <<"str2">> => <<"123">>,
                    <<"int">> => 123,
                    null => null
                   }},
            #{<<"inbox">> =>
                  #{<<"enabled">> => true,
                    <<"map">> => #{<<"value">> => 1},
                    <<"height">> => 100,
                    <<"filters">> =>
                        [#{<<"icon">> => <<"inbox">>,
                           <<"label">> => <<"Inbox">>
                          }
                        ]
                   }
             }]},
       eyaml:parse_file(FileName)).

parse_test8_test() ->
    FileName = test_file_path("test8.yaml"),
    {ok, Contents} = file:read_file(FileName),
    ?assertMatch({ok,[#{}]}, eyaml:parse(Contents)).

parse_fail_safe_test() ->
    FileName = test_file_path("test_schema0.yaml"),
    ?assertEqual(
       {ok,[#{<<"A null">> => <<"null">>,
              <<"Also a null">> => <<"">>,
              <<"Not a null">> => <<"">>,
              <<"Booleans">> =>
                  [<<"true">>, <<"True">>, <<"false">>, <<"FALSE">>],
              <<"Integers">> =>
                  [<<"0">>, <<"0o7">>, <<"0x3A">>, <<"-19">>],
              <<"Floats">> =>
                  [<<"0.">>, <<"-0.0">>, <<".5">>, <<"+12e03">>, <<"-2E+05">>],
              <<"Also floats">> =>
                  [<<".inf">>, <<"-.Inf">>, <<"+.INF">>, <<".NAN">>]
             }]},
       eyaml:parse_file(FileName, #{schema => fail_safe})).

parse_json_test() ->
    FileName = test_file_path("test_schema0.yaml"),
    ?assertEqual(
       {ok,[#{<<"A null">> => null,
              <<"Also a null">> => <<"">>,
              <<"Not a null">> => <<"">>,
              <<"Booleans">> =>
                  [true, <<"True">>, false, <<"FALSE">>],
              <<"Integers">> =>
                  [0, <<"0o7">>, <<"0x3A">>, -19],
              <<"Floats">> =>
                  [0.0, 0.0, 0.5, <<"+12e03">>, -2.0e5],
              <<"Also floats">> =>
                  [inf, <<"-.Inf">>, <<"+.INF">>, <<".NAN">>]
             }]},
       eyaml:parse_file(FileName, #{schema => json})).

parse_core_test() ->
    FileName = test_file_path("test_schema0.yaml"),
    ?assertEqual(
       {ok,[#{<<"A null">> => null,
              <<"Also a null">> => null,
              <<"Not a null">> => <<"">>,
              <<"Booleans">> =>
                  [true, true, false, false],
              <<"Integers">> =>
                  [0, 7, 16#3A, -19],
              <<"Floats">> =>
                  [0.0, 0.0, 0.5, 1.2e4, -2.0e5],
              <<"Also floats">> =>
                  [inf, '-inf', inf, nan]
             }]},
       eyaml:parse_file(FileName, #{schema => core})).
