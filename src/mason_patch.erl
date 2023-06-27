-module(mason_patch).

-export([patch/2, patch/3]).

%% JSON Patch
%% https://datatracker.ietf.org/doc/html/rfc6902/

-define(is_mpatch(X), (X =:= [] orelse is_map(hd(X)))).

patch(Patch, Subject) ->
    patch(Patch, Subject, #{}).

patch(Patch, Subject, O) when not ?is_mpatch(Patch) ->
    patch(mason:decode(Patch), Subject, O);
patch(Patch, Subject, O) when not ?is_mpatch(Subject) ->
    patch(Patch, mason:decode(Subject), O);
patch(Patch, Subject, O) ->
    try run(Patch, Subject)
    catch C:E -> debug(O, C, E, Subject)
    end.

%% returns the 4th argument (the subject), or does not return.
debug(#{debug := error}, C, E, S) -> error({patch_error, {C, E, S}});
debug(#{debug := log}, C, E, S) -> logger:info(#{error => {C, E}, subject => S}), S;
debug(_, _, _, S) -> S.

run([], S) -> S;
run([#{op := add, path := P, value := V}|Ps], S) ->
    run(Ps, add(path(P), V, S));
run([#{op := remove, path := P}|Ps], S) ->
    run(Ps, remove(path(P), S));
run([#{op := replace, path := P, value := V}|Ps], S) ->
    run(Ps, replace(path(P), V, S));
run([#{op := move, path := P, from := F}|Ps], S) ->
    run(Ps, move(path(P), path(F), S));
run([#{op := copy, path := P, from := F}|Ps], S) ->
    run(Ps, copy(path(P), path(F), S));
run([#{op := test, path := P, value := V}|Ps], S) ->
    run(Ps, test(path(P), V, S));
run([P|_], _) ->
    throw({bad_op, P}).

path(P) ->
    mason_pointer:parse(P).

%% add value V at path P.
%% if P exists, replace value with V.
add(_P, _V, S) -> S.

%% remove value at path P.
remove(_P, S) -> S.

%% replace value at path P with V.
%% fail if P does not exist.
replace(_P, _V, S) -> S.

%% move value at path F to path P.
move(_P, _F, S) -> S.

%% copy value at path F to path P.
copy(_P, _F, S) -> S.

%% compare value at path P with V. If they are equal, do nothing, else
%% fail.
test(_P, _V, _S) -> false.
