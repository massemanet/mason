-module(mason_path_test).

-include_lib("eunit/include/eunit.hrl").

assert_test_() ->
    wrap(
      fun assert/1,
      [{{["mason"], "json"},
        #{"mason" => "json"},
        true},
       {{["mason"], "bison"},
        #{"mason" => "json"},
        false},
       {{["mas", {["j"], "J"}, "on"], ["json"]},
        #{"mas" => [#{"j" => "H", "on" => "json"}]},
        false},
       {{["mas", {["j"], "J"}, "on"], ["son"]},
        #{"mas" => [#{"j" => "H", "on" => "json"}]},
        false},
       {{["mas", {["j"], "H"}, "on"], ["json"]},
        #{"mas" => [#{"j" => "H", "on" => "json"}]},
        true}]).

select_test_() ->
    wrap(
      fun select/1,
      [{["mason"],
        #{"mason" => "json"},
        "json"},
       {["mas", {["j"], "J"}, "on"],
        #{"mas" => [#{"j" => "J", "on" => "json"}]},
        ["json"]},
       {["mas", {["j"], "J"}, "on"],
        #{"mas" => [#{"j" => "H", "on" => "json"}]},
        []}]).

patch_test_() ->
    wrap(
      fun patch/1,
      [{[{"mason", "xml"}],
        #{"mason" => "json"},
        #{"mason" => "xml"}},
       {["ma", {"son", "xml"}],
        #{"ma" => #{"son" => "json"}},
        #{"ma" => #{"son" => "xml"}}},
       {["ma", {["son"], "json"}, {"good", "xml"}],
        #{"ma" => [#{"son" => "json"}, #{"son" => "son"}]},
        #{"ma" => [#{"son" => "json", "good" => "xml"}, #{"son" => "son"}]}}]).

wrap(F, Ts) ->
    [fun() -> F(T) end || T <- Ts].

assert({PXVal, M, Bool}) ->
    ?assertEqual(Bool, mason_path:assert(PXVal, M)).

select({PX, M, Val}) ->
    ?assertEqual(Val, mason_path:select(PX, M)).

patch({PX, M, Val}) ->
    ?assertEqual(Val, mason_path:patch(PX, M)).
