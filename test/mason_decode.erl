%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mason_decode).

-include_lib("eunit/include/eunit.hrl").

t_00_test() ->
    assert_eq(#{},
              '{}').
t_01_test() ->
    assert_eq(#{"a" => "b"},
              '{"a":"b"}').
t_02_test() ->
    assert_eq(#{"a" => <<0:8>>},
              '{"a":"0x00"}').
t_03_test() ->
    assert_eq(#{"a" => <<"b",1:8>>},
              '{"a":"0x6201"}').
t_04_test() ->
    assert_eq(#{"a" => <<"b",0:4>>},
              '{"a":"0x620"}').
t_05_test() ->
    assert_eq(#{"a" => 1},
              '{"a":1}').
t_06_test() ->
    assert_eq(#{"a" => 1.0},
              '{"a":1.0e+0}').
t_07_test() ->
    assert_eq(#{"a" => 1.0},
              '{"a":1e-0}').
t_08_test() ->
    assert_eq("a:b/1",
              '"a:b/1"').
t_09_test() ->
    assert_eq("<0.2.3>",
              '"<0.2.3>"').
t_12_test() ->
    assert_eq(["a", "b", "c"],
              '["a","b","c"]').
t_13_test() ->
    assert_eq([],
              '[  ]').
t_14_test() ->
    assert_eq([],
              '[]').
t_15_test() ->
    assert_eq(#{},
              ' {   }   ').
t_16_test() ->
    assert_eq(<<0:4,1:4,2:4,3:4,4:4,5:4,6:4,7:4,8:4,9:4,10:4,11:4,12:4,13:4,14:4,15:4>>,
              '"0x0123456789abcdef"').
t_17_test() ->
    assert_eq("127.0.0.1:666",
              '"127.0.0.1:666"').

assert_eq(Goal, In) ->
    ?assertEqual(Goal, mason:decode(atom_to_list(In))).
