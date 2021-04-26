-module(mason).

-export(
   [encode/1, encode/2,
    decode/1, decode/2]).

-export(
   [dec/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%API

encode(X) ->
    encode(X, #{}).

encode(X, Opts) ->
    store_opts(Opts),
    try lists:flatten(json(X))
    after delete_opts(Opts)
    end.

decode(JSON) ->
    decode(JSON, #{}).

decode(JSON, Opts) ->
    store_opts(Opts),
    try lift(mason_parser:parse(lift(mason_lexer:string(to_str(JSON)))))
    catch throw:Err -> Err
    after delete_opts(Opts)
    end.

to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_list(X) -> X.

%% decode json -> erlang
dec(val, number, {int, Str}) -> list_to_integer(Str);
dec(val, number, {float, Str}) -> list_to_float(Str);
dec(val, number, {intexp, Str}) -> intexp_to_float(Str);
dec(val, number, {floatexp, Str}) -> list_to_float(Str);
dec(val, string, {hex, Str}) -> hex_to_binary(Str);
dec(val, string, {chars, Str}) -> Str;
dec(key, Class, {Type, Str}) -> dec_key(Class, Type, Str);
dec(array, undefined, undefined) -> [];
dec(array, undefined, Val) -> [Val];
dec(array, Vals, Val) -> lists:reverse([Val|lists:reverse(Vals)]);
dec(object, undefined, undefined) -> #{};
dec(object, undefined, Member) -> Member;
dec(object, Members, Member) -> maps:merge(Members, Member).

dec_key(Class, Type, Str) ->
    case get_opt(keys) of
        undefined -> Str;
        atom -> list_to_atom(Str);
        string -> Str;
        binary -> list_to_binary(Str);
        term -> dec(val, Class, {Type, Str})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation

lift({ok, Val, _}) -> Val;
lift({ok, Val}) -> Val;
lift({error, {Line, mason_lexer, Err}, _}) -> throw({error, {lexer, Line, Err}});
lift({error, {Line, mason_parser, Err}}) -> throw({error, {syntax, Line, lists:flatten(Err)}}).

json(Bin)   when is_bitstring(Bin) -> emit_binary(Bin);
json(Atom)  when is_atom(Atom)     -> emit_atom(Atom);
json(N)     when is_number(N)      -> emit_number(N);
json(Pid)   when is_pid(Pid)       -> emit_pid(Pid);
json(Ref)   when is_reference(Ref) -> emit_ref(Ref);
json(Fun)   when is_function(Fun)  -> emit_fun(Fun);
json(Port)  when is_port(Port)     -> emit_port(Port);
json(List)  when is_list(List)     -> emit_list(List);
json(Tuple) when is_tuple(Tuple)   -> emit_tuple(Tuple);
json(Map)   when is_map(Map)       -> emit_map(Map).

emit_binary(Bin) ->
    emit_binary(Bin, "", "").

-define(ipp(_A, _B, _C, _D, _P), {{_A, _B, _C, _D}, _P}).
-define(ip(_A, _B, _C, _D), {_A, _B, _C, _D}).
-define(mfa(_M, _F, _A), {_M, _F, _A}).
-define(stack(_M, _F, _A, _L), {_M, _F, _A, [_, {line, _L}]}).
-define(is_byte(_B), 0 =< _B, _B =< 255).
-define(is_long(_L), 0 =< _L, _L =< 65535).
-define(is_ipp(_A, _B, _C, _D, _P), ?is_ip(_A, _B, _C, _D), ?is_long(_P)).
-define(is_ip(_A, _B, _C, _D), ?is_byte(_A), ?is_byte(_B), ?is_byte(_C), ?is_byte(_D)).
-define(is_mfa(_M, _F, _A), is_atom(_M), is_atom(_F), is_integer(_A)).
-define(is_stack(_M, _F, _A, _L), ?is_mfa(_M, _F, _A), is_integer(_L)).
-define(is_printable(C), C==9; C==10; C==13; 32 =< C andalso C=< 126).
-define(is_proplist(_T), tuple_size(_T) =:= 2, is_atom(element(1, _T))).

%% we encode to "0xcafe" and "Str" in parallel. If both work we keep
%% "Str".
emit_binary(<<>>, undefined, Hex) ->
    wrap(["0x", lists:reverse(Hex)]);
emit_binary(<<>>, Str, _) ->
    wrap(lists:reverse(Str));
emit_binary(<<A:1>>, _, Hex) ->
    emit_binary(<<>>, undefined, [hex(A)|Hex]);
emit_binary(<<A:2>>, _, Hex) ->
    emit_binary(<<>>, undefined, [hex(A)|Hex]);
emit_binary(<<A:3>>, _, Hex) ->
    emit_binary(<<>>, undefined, [hex(A)|Hex]);
emit_binary(<<A:4, T/bitstring>>, undefined, Hex) ->
    emit_binary(T, undefined, [hex(A)|Hex]);
emit_binary(<<I:8, T/binary>>, Str, Hex) when ?is_printable(I) ->
    emit_binary(T, [I|Str], [hex(I rem 16), hex(I div 16)|Hex]);
emit_binary(<<A:4, T/bitstring>>, _, Hex) ->
    emit_binary(T, undefined, [hex(A)|Hex]).

%% `true', `false', and `null' are json keywords
emit_atom(true) -> "true";
emit_atom(false) -> "false";
emit_atom(undefined) -> "null";
emit_atom(A) -> wrap(atom_to_list(A)).

%% numbers should be fine and not quoted
emit_number(I) when is_integer(I) ->
    integer_to_list(I);
emit_number(F) ->
    float_to_list(F).

%% just stringify
emit_pid(Pid) ->
    wrap(pid_to_list(Pid)).

%% just stringify
emit_ref(Ref) ->
    wrap(ref_to_list(Ref)).

%% present as an MFA
emit_fun(Fun) ->
    emit_tuple({fun_info(Fun, module), fun_info(Fun, name), fun_info(Fun, arity)}).

%% show some interesting info
emit_port(Port) ->
    case port_info(Port, name) of
        "tcp_inet" -> wrap(["Port(tcp:", inet_info(Port, port), ")"]);
        "udp_inet" -> wrap(["Port(udp:", inet_info(Port, port), ")"]);
        "sctp_inet" -> wrap(["Port(sctp:", inet_info(Port, port), ")"]);
        undefined -> wrap("Port(dead)");
        Name -> wrap(["Port(", Name, ")"])
    end.

%% a list is either a string, a proplist, or just a list. we map to
%% a string, an object, or a list, respectively
emit_list(List) ->
    case List of
        [] -> "[]";
        [PL|_] when ?is_proplist(PL) -> emit_list(List, {obj, []}, []);
        [I|_] when ?is_printable(I) -> emit_list(List, {str, ""}, []);
        _ -> emit_list(List, undefined, [])
    end.

emit_list([], undefined, R) ->
    ["[", tl(lists:reverse(R)), "]"];
emit_list([], {obj, O}, _) ->
    ["{", tl(lists:reverse(O)), "}"];
emit_list([], {str, S}, _) ->
    wrap(lists:reverse(S));
emit_list([{K, V}|T], {obj, O}, R) when ?is_proplist({K, V}) ->
    emit_list(T, {obj, [json(V), ":", json(K), ","|O]}, [emit_tuple({K, V}), ","|R]);
emit_list([I|T], {str, S}, R) when ?is_printable(I) ->
    emit_list(T, {str, [I|S]}, [emit_number(I), ","|R]);
emit_list([E|T], _, R) ->
    emit_list(T, undefined, [json(E), ","|R]).

%% we map tuples to json lists, except IP numbers, {IP, Port}, stack traces, and MFAs.
emit_tuple(?stack(M, F, A, L)) when ?is_stack(M, F, A, L) ->
    wrap([mfa_(M, F, A), ":", integer_to_list(L)]);
emit_tuple(?ipp(A, B, C, D, Port)) when ?is_ipp(A, B, C, D, Port) ->
    wrap([ip(A, B, C, D), ":", integer_to_list(Port)]);
emit_tuple(?ip(A, B, C, D)) when ?is_ip(A, B, C, D)->
    wrap(ip(A, B, C, D));
emit_tuple(?mfa(M, F, A)) when ?is_mfa(M, F, A) ->
    wrap(mfa_(M, F, A));
emit_tuple(T) ->
    emit_list(tuple_to_list(T)).

%% we map maps to json objects
emit_map(Map) when map_size(Map) =:= 0 ->
    "{}";
emit_map(Map) ->
    ["{", tl(lists:reverse(maps:fold(fun emit_map/3, [], Map))), "}"].

emit_map(K, V, O) ->
    [json(V), ":", json(K), ","|O].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

hex(I) when I < 10 -> I+$0;
hex(I) -> I+$W.

ip(I1, I2, I3, I4) ->
    string:join([integer_to_list(I) || I <- [I1, I2, I3, I4]], ".").

mfa_(M, F, A) ->
    [atom_to_list(M), ":", atom_to_list(F), "/", integer_to_list(A)].

fun_info(Fun, Tag) ->
    {Tag, Val} = erlang:fun_info(Fun, Tag),
    Val.

port_info(Port, Tag) ->
    try {Tag, Val} = erlang:port_info(Port, Tag), Val catch _:_ -> undefined end.

inet_info(Port, port) ->
    try {ok, Val} = inet:port(Port), integer_to_list(Val) catch _:_ -> "" end.

wrap(X) ->
    [$", X, $"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decoding json -> erlang

intexp_to_float(S) ->
    [A,B] = re:split(S, "[Ee]", [{return, list}]),
    list_to_float(lists:append([A,".0e", B])).

hex_to_binary("0x"++R) ->
    hex_to_binary(R, <<>>).

-define(upcase(C), $A =< C, C =< $F).
-define(lowcase(C), $a =< C, C =< $f).
-define(digit(C), $0 =< C, C =< $9).
hex_to_binary("", O) ->
    O;
hex_to_binary([C|R], O) when ?upcase(C) ->
    hex_to_binary(R, <<O/bitstring, (C-$A+10):4>>);
hex_to_binary([C|R], O) when ?lowcase(C) ->
    hex_to_binary(R, <<O/bitstring, (C-$a+10):4>>);
hex_to_binary([C|R], O) when ?digit(C) ->
    hex_to_binary(R, <<O/bitstring, (C-$0):4>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options

store_opts(Opts) ->
    maps:map(fun(K, V) -> put({mason, K}, V) end, Opts).

delete_opts(Opts) ->
    maps:map(fun(K, _) -> erase({mason, K}) end, Opts).

get_opt(K) ->
    get({mason, K}).

%% maybe_ts(TS, Conf) ->
%%     case Conf of
%%         #{ts := none} -> "";
%%         _ -> ["\"ts\": \"", ts(TS), "\", "]
%%     end.

%% ts(TS) ->
%%     Sec = TS div 1000_000,
%%     Usec = TS rem 1000_000,
%%     {{Yr, Mo, Dy}, {H, M, S}} = unix_secs_to_datetime(Sec),
%%     [pad(4, Yr), "-", pad(2, Mo), "-", pad(2, Dy), $T,
%%      pad(2, H), ":", pad(2, M), ":", pad(2, S), $., integer_to_list(Usec)].
%% unix_secs_to_datetime(Sec) ->
%%     GregorianSecs1970 = 62167219200,
%%     calendar:gregorian_seconds_to_datetime(Sec+GregorianSecs1970).

%% pad(_, N) when N < 0 -> error({negative_time, N});
%% pad(L, N) when is_integer(N) -> pad(L, integer_to_list(N));
%% pad(4, [_, _, _, _] = S) -> S;
%% pad(4, [_, _, _] = S) -> [$0, S];
%% pad(4, [_, _] = S) -> [$0, $0, S];
%% pad(4, [_] = S) -> [$0, $0, $0, S];
%% pad(2, [_, _] = S) -> S;
%% pad(2, [_] = S) -> [$0, S].

%% name(Pid) ->
%%     case pname(Pid) of
%%         {M, F, A} -> io_lib:format("~w:~w/~w(~w)", [M, F, A, Pid]);
%%         X -> io_lib:format("~w(~w)", [X, Pid])
%%     end.

%% pname(Pid) ->
%%     case process_info(Pid, registered_name) of
%%         {registered_name, Reg} -> Reg;
%%         undefined -> dead;
%%         [] -> initial_call(Pid)
%%     end.

%% initial_call(Pid) ->
%%     case process_info(Pid, initial_call) of
%%         {_, {proc_lib, init_p, 5}} -> proc_lib:translate_initial_call(Pid);
%%         {_, MFA}  -> MFA;
%%         undefined -> dead
%%    end.
