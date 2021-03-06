%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mason_decoder).

-export([go/3]).

%% decode json -> erlang. this is called from the parser. we're supposed to turn
%% values into erlang terms (all values are in string representation), arrays
%% into lists (or tuples), and objects into maps (or proplists). we use
%% `mason:get_opt' to get guidance.
go(val, number, {int, Str}) -> list_to_integer(Str);
go(val, number, {float, Str}) -> list_to_float(Str);
go(val, number, {intexp, Str}) -> intexp_to_float(Str);
go(val, number, {floatexp, Str}) -> list_to_float(Str);
go(val, string, {hex, Str}) -> hex_to_binary(Str);
go(val, string, {chars, Str}) -> dec_string(Str);
go(val, string, {time, Str}) -> ts(Str);
go(val, word, true) -> true;
go(val, word, false) -> false;
go(val, word, null) -> dec_null();
go(key, Class, {Type, Str}) -> dec_key(Class, Type, Str);
go(array, [], []) -> [];
go(array, [], Val) -> [Val];
go(array, Vals, Val) -> lists:reverse([Val|lists:reverse(Vals)]);
go(object, [], []) -> #{};
go(object, [], Member) -> Member;
go(object, Members, Member) -> maps:merge(Members, Member).

dec_key(Class, Type, Str) ->
    case mason:get_opt(keys, string) of
        atom -> list_to_atom(Str);
        string -> Str;
        binary -> list_to_binary(Str);
        term -> go(val, Class, {Type, Str})
    end.

dec_string(Str) ->
    case mason:get_opt(string, string) of
        string -> Str;
        binary -> list_to_binary(Str);
        atom -> list_to_atom(Str)
    end.

dec_null() ->
    case mason:get_opt(null, undefined) of
        undefined -> undefined;
        null -> null
    end.

%% kind of rfc 3339. we allow compact and verbose datetime, and frac.
%% we drop the timezone.
ts([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$T,H1,H2,$:,M00,M01,$:,S1,S2|R]) ->
    M = [Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$T,H1,H2,$:,M00,M01,$:,S1,S2],
    maybe_frac(M, R);
ts([Y1,Y2,Y3,Y4,M1,M2,D1,D2,$T,H1,H2,M00,M01,S1,S2|R]) ->
    M = [Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$T,H1,H2,$:,M00,M01,$:,S1,S2],
    maybe_frac(M, R).

maybe_frac(M, [$.|R]) ->
    M ++ [$.] ++ digits(R);
maybe_frac(M, _) ->
    M.

digits([H|T]) when $0 =< H, H =< $9 ->
    [H|digits(T)];
digits(_) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% integer with exponent; e.g. "1e3"

intexp_to_float(S) ->
    [A,B] = re:split(S, "[Ee]", [{return, list}]),
    list_to_float(lists:append([A,".0e", B])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hexstring, e.g. "0xa3:2"

hex_to_binary("0x"++R) ->
    hex_to_binary(R, 4, <<>>).

-define(upcase(C), $A =< C, C =< $F).
-define(lowcase(C), $a =< C, C =< $f).
-define(digit(C), $0 =< C, C =< $9).
hex_to_binary("", _, O) ->
    O;
hex_to_binary([C, $:, N], _, O) when $0 < N, N < $4 ->
    hex_to_binary([C], N-$0, O);
hex_to_binary([C|R], N, O) when ?upcase(C) ->
    hex_to_binary(R, N, <<O/bitstring, (C-$A+10):N>>);
hex_to_binary([C|R], N, O) when ?lowcase(C) ->
    hex_to_binary(R, N, <<O/bitstring, (C-$a+10):N>>);
hex_to_binary([C|R], N, O) when ?digit(C) ->
    hex_to_binary(R, N, <<O/bitstring, (C-$0):N>>).
