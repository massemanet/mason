-module(mason).

-export(
   [encode/1,
    encode/2]).

encode(X) ->
    encode(X, #{}).

encode(X, _Opts) ->
    lists:flatten(json(X)).

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

-define(is_printable(C), C==9; C==10; C==13; 32 =< C andalso C=< 126).

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
        [{_, _}|_] -> emit_list(List, {obj, []}, []);
        [I|_] when ?is_printable(I) -> emit_list(List, {str, ""}, []);
        _ -> emit_list(List, undefined, [])
    end.

emit_list([], undefined, R) ->
    ["[", tl(lists:reverse(R)), "]"];
emit_list([], {obj, O}, _) ->
    ["{", tl(lists:reverse(O)), "}"];
emit_list([], {str, S}, _) ->
    wrap(lists:reverse(S));
emit_list([{K, V}|T], {obj, O}, R) ->
    emit_list(T, {obj, [json(V), ":", json(K), ","|O]}, [emit_tuple({K, V})|R]);
emit_list([I|T], {str, S}, R) when ?is_printable(I) ->
    emit_list(T, {str, [I|S]}, [emit_number(I)|R]);
emit_list([E|T], _, R) ->
    emit_list(T, undefined, [json(E), ","|R]).

-define(EI(_E, _T), 0 =< element(_E, _T), element(_E, _T) =< 255).
-define(EA(_E, _T), is_atom(element(_E, _T))).
-define(is_ip(_IP), tuple_size(_IP) == 4, ?EI(1, _IP), ?EI(2, _IP), ?EI(3, _IP), ?EI(4, _IP)).
-define(is_mfa(_MFA), tuple_size(_MFA) == 3, ?EA(1, _MFA), ?EA(2, _MFA), ?EI(3, _MFA)).

%% we map tuples to json lists, except IP numbers and MFAs.
emit_tuple(IP) when ?is_ip(IP)->
    wrap(string:join([integer_to_list(I) || I <- tuple_to_list(IP)], "."));
emit_tuple(MFA) when ?is_mfa(MFA) ->
    {M, F, A} = MFA,
    wrap([atom_to_list(M), ":", atom_to_list(F), "/", integer_to_list(A)]);
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
hex(I) -> I+$w.

fun_info(Fun, Tag) ->
    {Tag, Val} = erlang:fun_info(Fun, Tag),
    Val.

port_info(Port, Tag) ->
    try {Tag, Val} = erlang:port_info(Port, Tag), Val catch _:_ -> undefined end.

inet_info(Port, port) ->
    try {ok, Val} = inet:port(Port), integer_to_list(Val) catch _:_ -> "" end.

wrap(X) ->
    [$", X, $"].

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
