-module(mason_patch_test).

-include_lib("eunit/include/eunit.hrl").

assert_test_() ->
    wrap(
      fun assert/1,
      [{'[{ "op": "replace", "path": "/baz", "value": "boo" },
          { "op": "add", "path": "/hello", "value": ["world"] },
          { "op": "remove", "path": "/foo"}]',
        '{"baz": "qux", "foo": "bar"}',
        '{"baz": "boo", "hello": ["world"]}'}]).

wrap(F, Ts) ->
    [fun() -> F(T) end || T <- Ts].

assert({Patch, Subject, Expected}) ->
    ?assertEqual(mason:decode(Expected), mason_patch:patch(Patch, Subject)).
