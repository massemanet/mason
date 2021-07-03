%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mason_encoder).

-export([emit/1,
         ts/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the erlang types

emit(Bin)   when is_bitstring(Bin) -> emit_binary(Bin);
emit(Atom)  when is_atom(Atom)     -> emit_atom(Atom);
emit(N)     when is_number(N)      -> emit_number(N);
emit(Pid)   when is_pid(Pid)       -> emit_pid(Pid);
emit(Ref)   when is_reference(Ref) -> emit_ref(Ref);
emit(Fun)   when is_function(Fun)  -> emit_fun(Fun);
emit(Port)  when is_port(Port)     -> emit_port(Port);
emit(List)  when is_list(List)     -> emit_list(List);
emit(Tuple) when is_tuple(Tuple)   -> emit_tuple(Tuple);
emit(Map)   when is_map(Map)       -> emit_map(Map).

%% some abstract patterns
-define(ip(A, B, C, D), {A, B, C, D}).
-define(ipp(A, B, C, D, P), {{A, B, C, D}, P}).
-define(mfa(M, F, A), {M, F, A}).
-define(stack(M, F, A, L), {M, F, A, [_, {line, L}]}).

%% some user-defined guards
-define(nneg(I), is_integer(I), 0 =< I).
-define(e(N, T), element(N, T)).
-define(ee(N, M, T), element(N, element(M, T))).
-define(ts(S, T), tuple_size(T) =:= S).
-define(is_byte(B), 0 =< B, B =< 255).
-define(is_long(L), 0 =< L, L =< 65535).
-define(is_ipp(A, B, C, D, P), ?is_ip(A, B, C, D), ?is_long(P)).
-define(is_ip(A, B, C, D), ?is_byte(A), ?is_byte(B), ?is_byte(C), ?is_byte(D)).
-define(is_mfa(M, F, A), is_atom(M), is_atom(F), ?nneg(A)).
-define(is_stack(M, F, A, L), ?is_mfa(M, F, A), ?nneg(L)).
-define(is_proplist(T), tuple_size(T) =:= 2, is_atom(element(1, T))).
-define(is_printable(C), C==9; C==10; C==13; 32 =< C andalso C=< 126).
-define(is_datetime(T),
        ?ts(2, T), ?ts(3, ?e(1, T)), ?ts(3, ?e(2, T)),
        ?nneg(?ee(1, 1, T)), ?nneg(?ee(2, 1, T)), ?nneg(?ee(3, 1, T)),
        ?nneg(?ee(1, 2, T)), ?nneg(?ee(2, 2, T)), ?nneg(?ee(3, 2, T))).

%% erlang binary() is encoded either as a text string or as a "0x" string

emit_binary(Bin) ->
    emit_binary(Bin, "", "").

%% we encode to "0xcafe" and "Str" in parallel. If both work we keep
%% "Str".
emit_binary(<<>>, undefined, Hex) ->
    wrap(["0x", lists:reverse(Hex)]);
emit_binary(<<>>, Str, _) ->
    wrap(lists:reverse(Str));
emit_binary(<<A:1>>, _, Hex) ->
    emit_binary(<<>>, undefined, [$1, $:, hex(A)|Hex]);
emit_binary(<<A:2>>, _, Hex) ->
    emit_binary(<<>>, undefined, [$2, $:, hex(A)|Hex]);
emit_binary(<<A:3>>, _, Hex) ->
    emit_binary(<<>>, undefined, [$3, $:, hex(A)|Hex]);
emit_binary(<<A:4, T/bitstring>>, undefined, Hex) ->
    emit_binary(T, undefined, [hex(A)|Hex]);
emit_binary(<<I:8, T/binary>>, Str, Hex) when ?is_printable(I) ->
    emit_binary(T, escape(I, Str), [hex(I rem 16), hex(I div 16)|Hex]);
emit_binary(<<A:4, T/bitstring>>, _, Hex) ->
    emit_binary(T, undefined, [hex(A)|Hex]).

%% erlang atom() is encoded as string, except `true', `false', and `undefined',
%% which are encoded as the json words `true', `false', and ``null',
emit_atom(true) -> "true";
emit_atom(false) -> "false";
emit_atom(undefined) -> emit_undefined();
emit_atom(A) -> wrap(escape(A)).

emit_undefined() ->
    case mason:get_opt(undefined, undefined) of
        null -> "null";
        undefined -> wrap("undefined")
    end.

%% numbers should be fine and not quoted
emit_number(I) when is_integer(I) ->
    integer_to_list(I);
emit_number(F) ->
    float_to_list(F).

%% erlang pid(), decorate with name
emit_pid(Pid) ->
    wrap(name(Pid)).

name(Pid) ->
    case pname(Pid) of
        {M, F, A} -> io_lib:format("~w:~w/~w(~w)", [M, F, A, Pid]);
        X -> io_lib:format("~w(~w)", [X, Pid])
    end.

pname(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Reg} -> Reg;
        undefined -> dead;
        [] -> initial_call(Pid)
    end.

initial_call(Pid) ->
    case process_info(Pid, initial_call) of
        {_, {proc_lib, init_p, 5}} -> proc_lib:translate_initial_call(Pid);
        {_, {erlang, apply, 2}} -> callstack(Pid);
        {_, MFA}  -> MFA;
        undefined -> dead
    end.

callstack(Pid) ->
    try process_info(Pid, current_stacktrace) of
        undefined -> dead;
        {_, []} -> {erlang,apply,2};
        {_, MFAs} ->
            case lists:reverse(MFAs) of
                [{M, F, A, _}|_] -> {M, F, A};
                [{M, F, A}|_] -> {M, F, A};
                _ -> {erlang, apply, 2}
            end
    catch error:badarg -> {erlang, apply, 2}
    end.

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
    emit_list(T, {obj, [emit(V), ":", emit(K), ","|O]}, [emit_tuple({K, V}), ","|R]);
emit_list([I|T], {str, S}, R) when ?is_printable(I) ->
    emit_list(T, {str, escape(I, S)}, [emit_number(I), ","|R]);
emit_list([E|T], _, R) ->
    emit_list(T, undefined, [emit(E), ","|R]).

%% we map tuples to json lists, except IP numbers, {IP, Port}, stack traces, and MFAs.
emit_tuple(?stack(M, F, A, L)) when ?is_stack(M, F, A, L) ->
    wrap([emit_mfa(M, F, A), ":", integer_to_list(L)]);
emit_tuple(?ipp(A, B, C, D, Port)) when ?is_ipp(A, B, C, D, Port) ->
    wrap([ip(A, B, C, D), ":", integer_to_list(Port)]);
emit_tuple(?ip(A, B, C, D)) when ?is_ip(A, B, C, D)->
    wrap(ip(A, B, C, D));
emit_tuple(?mfa(M, F, A)) when ?is_mfa(M, F, A) ->
    wrap(emit_mfa(M, F, A));
emit_tuple(DateTime) when ?is_datetime(DateTime) ->
    wrap(ts(DateTime, datetime));
emit_tuple(T) ->
    case record_keys(T) of
        [] -> emit_list(tuple_to_list(T));
        Keys -> emit_list(lists:zip(['RECORD'|Keys], tuple_to_list(T)))
    end.

record_keys(T) ->
    case 1 < (TS = tuple_size(T)) of
        true -> mason:record_keys(element(1, T), TS);
        false -> []
    end.

emit_mfa(M, F, A) ->
    [escape(M), ":", escape(F), "/", integer_to_list(A)].

%% we map maps to json objects
emit_map(Map) when map_size(Map) =:= 0 ->
    "{}";
emit_map(Map) ->
    ["{", tl(lists:reverse(maps:fold(fun emit_map/3, [], Map))), "}"].

emit_map(K, V, O) ->
    [emit(V), ":", emit(K), ","|O].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

hex(I) when I < 10 -> I+$0;
hex(I) -> I+$W.

ip(I1, I2, I3, I4) ->
    string:join([integer_to_list(I) || I <- [I1, I2, I3, I4]], ".").

fun_info(Fun, Tag) ->
    {Tag, Val} = erlang:fun_info(Fun, Tag),
    Val.

port_info(Port, Tag) ->
    try {Tag, Val} = erlang:port_info(Port, Tag), Val catch _:_ -> undefined end.

inet_info(Port, port) ->
    try {ok, Val} = inet:port(Port), integer_to_list(Val) catch _:_ -> "" end.

wrap(X) ->
    [$", X, $"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% string escapes

escape(Atom) when is_atom(Atom) ->
    lists:reverse(lists:foldl(fun escape/2, "", atom_to_list(Atom))).

escape(I, Str) ->
    case I of
        $\\ ->[$\\, $\\|Str];
        $"  -> [$", $\\|Str];
        $\n -> [$n, $\\|Str];
        $\t -> [$t, $\\|Str];
        $\r -> [$r, $\\|Str];
        _   -> [I|Str]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timestamp

ts({MS, S, US}, now) -> ts(MS*1000_000+S+(US/1000_000), sec);
ts(TS, nsec) ->  ts(TS/1000_000_000, sec);
ts(TS, usec) ->  ts(TS/1000_000, sec);
ts(TS, msec) ->  ts(TS/1000, sec);
ts(TS, sec) ->
    Sec = trunc(TS),
    {{Yr, Mo, Dy}, {H, M, S}} = unix_secs_to_datetime(Sec),
    ts(Yr, Mo, Dy, H, M, S+TS-Sec);
ts({{Yr, Mo, Dy}, {H, M, S}}, datetime) ->
    ts(Yr, Mo, Dy, H, M, S).

ts(Yr, Mo, Dy, H, M, Sec) ->
    S = trunc(Sec),
    lists:flatten(
      [pad(4, Yr), "-", pad(2, Mo), "-", pad(2, Dy),
       "T",
       pad(2, H), ":", pad(2, M), ":", pad(2, S),
       frac(Sec-S),
       "Z"]).

frac(F) when F < 0.5e-9 -> [];
frac(F) -> tl(float_to_list(F, [{decimals, 9}, compact])).

unix_secs_to_datetime(Sec) ->
    GregorianSecs1970 = 62167219200,
    calendar:gregorian_seconds_to_datetime(Sec+GregorianSecs1970).

pad(_, N) when N < 0 -> error({negative_time, N});
pad(L, N) when is_integer(N) -> pad(L, integer_to_list(N));
pad(4, [_, _, _, _] = S) -> S;
pad(4, [_, _, _] = S) -> [$0, S];
pad(4, [_, _] = S) -> [$0, $0, S];
pad(4, [_] = S) -> [$0, $0, $0, S];
pad(2, [_, _] = S) -> S;
pad(2, [_] = S) -> [$0, S].
