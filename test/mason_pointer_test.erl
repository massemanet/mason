-module(mason_pointer_test).

-include_lib("eunit/include/eunit.hrl").

assert_test_() ->
    wrap(
      fun rfc6901/1,
      [{"", mason:decode(val())},
       {"/foo", ["bar", "baz"]},
       {"/foo/0", "bar"},
       {"/", 0},
       {"/a~1b", 1},
       {"/c%d", 2},
       {"/e^f", 3},
       {"/g|h", 4},
       {"/i\\j", 5},
       {"/k\"l", 6},
       {"/ ", 7},
       {"/m~0n", 8}]).

wrap(F, Ts) ->
    [fun() -> F(T) end || T <- Ts].

rfc6901({Ptr, Expected}) ->
    ?assertEqual(Expected, mason_pointer:value(Ptr, val())).

%% example from spec
val() ->
    '{
       "foo": ["bar", "baz"],
       "": 0,
       "a/b": 1,
       "c%d": 2,
       "e^f": 3,
       "g|h": 4,
       "i\\\\j": 5,
       "k\\\"l": 6,
       " ": 7,
       "m~n": 8
     }'.
