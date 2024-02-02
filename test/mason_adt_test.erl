-module(mason_adt_test).


-include_lib("eunit/include/eunit.hrl").

t_00_test() ->
    ?assertEqual(
       mason_adt:mason(#{a => b}),
       [#{<<"a">> => <<"b">>}]).
