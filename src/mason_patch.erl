-module(mason_patch).

-export([patch/2, patch/3]).

%% JSON Patch
%% https://datatracker.ietf.org/doc/html/rfc6902/

-define(is_mterm(X), (is_list(X) orelse is_map(X))).

patch(Patch, Subject) ->
    patch(Patch, Subject, #{}).

patch(Patch, Subject, O) when not ?is_mterm(Patch) ->
    patch(mason:decode(Patch), Subject, O);
patch(Patch, Subject, O) when not ?is_mterm(Subject) ->
    patch(Patch, mason:decode(Subject), O);
patch(Patch, Subject, O) ->
    try run(Patch, Subject)
    catch C:E -> debug(O, {C, E}, Subject)
    end.

%% returns the 3rd argument (the subject), or does not return.
debug(#{debug := error}, CE, S) ->
    error({patch_error, CE, S});
debug(#{debug := log}, CE, S) ->
    logger:info(#{error => CE, subject => S}), S;
debug(_, _, S) ->
    S.

%% run all ops. An op will either return a new Subject, or throw.
run([], S) ->
    S;
run([#{"op" := "add", "path" := P, "value" := V}|Ops], S) ->
    run(Ops, add(P, V, S));
run([#{"op" := "remove", "path" := P}|Ops], S) ->
    run(Ops, rm(P, S));
run([#{"op" := "replace", "path" := P, "value" := V}|Ops], S) ->
    run(Ops, replace(P, V, S));
run([#{"op" := "move", "path" := P, "from" := F}|Ops], S) ->
    run(Ops, add(P, ptr_value(F, S), rm(F, S)));
run([#{"op" := "copy", "path" := P, "from" := F}|Ops], S) ->
    run(Ops, add(P, ptr_value(F, S), S));
run([#{"op" := "test", "path" := P, "value" := V}|Ops], S) ->
    run(Ops, test(P, V, S));
run([Op|_], _) ->
    throw({bad_op, Op}).

%% add value V at path P.
%% if P exists, replace value with V.
add(P, V, S) ->
    walk(ptr_path(P), mk_add(V), S).

mk_add(V) ->
    fun(P, S) when is_map(S) -> S#{P => V};
       (H, []) -> H++[V];
       (H, [_|T]) -> H++[V|T]
    end.

%% remove value at path P.
%% fail if P does not exist.
rm(P, S) ->
    walk(ptr_path(P), mk_rm(), S).

mk_rm() ->
    fun(P, S) when is_map_key(P, S) -> map_rm(P, S);
       (H, [_|T]) -> H++T;
       (A, B) -> throw({remove_fail, {A, B}})
    end.

%% replace value at path P with V.
%% fail if P does not exist.
replace(P, V, S) ->
    walk(ptr_path(P), mk_replace(V), S).

mk_replace(V) ->
    fun(P, S) when is_map_key(P, S) -> S#{P => V};
       (H, [_|T]) -> H++[V|T];
       (A, B) -> throw({replace_fail, {V, A, B}})
    end.

%% compare value at path P with V. If they are equal, return S, else
%% fail.
test(P, V, S) ->
    case ptr_value(P, S) of
        V -> S;
        W -> throw({compare_fail, {V, W}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ptr_path(P) ->
    try mason_pointer:parse(P)
    catch C:E -> throw({path_fail, {C, E, P}})
    end.

ptr_value(P, S) ->
    try mason_pointer:value(P, S)
    catch _:_ -> throw({not_found, {P, S}})
    end.

%% follow the Path through a Subject. When we get to the end of the
%% Path, we call V (a fun/2). Note that all path elements must exist,
%% except the last one.
walk([P], V, S) when is_map(S) ->
    V(P, S);
walk([P|Ps], V, S) when is_map(S) ->
    S#{P => walk(Ps, V, map_val(P, S))};
walk([P], V, S) when is_list(S) ->
    {H, T} = split(P, S),
    V(H, T);
walk([P|Ps], V, S) when is_list(S) ->
    case split(P, S) of
        {_, []}  -> throw({oob_fail, {P, S}});
        {H, [Z|T]} -> H++[walk(Ps, V, Z)|T]
    end;
walk([], _, S) ->
    throw({walk_fail, S}).

%% wrap maps:get/2
map_val(K, M) ->
    try maps:get(K, M)
    catch _:_ -> throw({take_fail, {K, M}})
    end.

%% wrap maps:take/2
map_rm(K, M) ->
    case maps:take(K, M) of
        error -> throw({remove_fail, {K, M}});
        {_, O} -> O
    end.

split(P, S) ->
    I = index(P),
    case length(S) of
        L when I =< L -> lists:split(I, S);
        L -> throw({bounds_fail, {I, L}})
    end.

index(P) ->
    try list_to_integer(P)
    catch _:_ -> throw({index_fail, P})
    end.
