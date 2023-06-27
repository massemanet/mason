-module(mason_pointer).

-export(
   [parse/1,
    value/2]).

%% JSON Pointer
%% https://datatracker.ietf.org/doc/html/rfc6901

value(Ptr, Subject) ->
    dig(parse(Ptr), mason:decode(Subject)).

dig([], S) ->
    S;
dig([P|Ps], S) when is_map(S) ->
    try dig(Ps, maps:get(P, S))
    catch C:E:Sz -> error({not_found, P, S, C, E, hd(Sz)})
    end;
dig([P|Ps], S) when is_list(S) ->
    N = index(P),
    case length(S) of
        L when N < L -> dig(Ps, lists:nth(N, S));
        L -> error({badarg, {out_of_bounds, N, L}})
    end.

%% If the currently referenced value is a JSON array, the reference
%% token MUST contain either: [an integer, or] exactly the single
%% character "-", making the new referenced value the (nonexistent)
%% member after the last array element.
index("-") ->
    error({badarg, {out_of_bounds, "-"}});
index(P) ->
    try list_to_integer(P)+1
    catch _:_ -> error({badarg, {expected_int, P}})
    end.

parse(X) when is_atom(X) -> parse(atom_to_list(X));
parse(X) when is_binary(X) -> parse(binary_to_list(X));
parse(X) when not is_list(X) -> error({badarg, X});
parse(X) when not ($/ =:= hd(X)) -> error({badarg, X});
parse(X) -> parse(X, []).

parse([$/|T], []) -> parse(T, [""]);
parse([$/|T], [S|Ss]) -> parse(T, ["", lists:reverse(S)|Ss]);
parse([$~, $0|T], [S|Ss]) -> parse(T, [[$~|S]|Ss]);
parse([$~, $1|T], [S|Ss]) -> parse(T, [[$/|S]|Ss]);
parse([C|T], [S|Ss]) -> parse(T, [[C|S]|Ss]);
parse([], [S|Ss]) -> lists:reverse([lists:reverse(S)|Ss]);
parse([], []) -> [].
