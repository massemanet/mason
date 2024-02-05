-module(mason_adt).

%% API
-export [file/1, mason/1, unmason/1, json/1].
%% generic functions
-export [fold/3, walk/2, sizeof/1, type/1, merge/2, umerge/2, eq/2, member/2, singlet/1].
%% str
-export [str/1].
%% pair
-export [pair/2, key/1, val/1, kv/1].
%% props
-export [props/1, kvs/1, take/2, mutate/3, peek/2, peek/3, path/2, add/2, rm/2].
%% array
-export [array/1, cons/2, decons/1, head/1, tail/1, nth/2, all_is/2].

%% This module defines an abstract data type; Mason(). Inspired
%% by JSON, a Mason() is either a primitive (Str(), Bool(),
%% Num(), or Null()), a Props() (a set of Pair()), or an Array (an
%% ordered set). A Pair() is pair of <Str(), Mason()>.
%%
%% The module exports the main functions;
%%   * mason/1 - turn an erlang term() into a Mason().
%%   * walk/2  - fold a Mason() over a fun/2.
%%   * json/1  - pretty print a Mason() as json.
%%
%% The primitive types;
-type str() :: binary().
-type num() :: float() | integer().
-type boo() :: true | false.
-type nul() :: null.
%%
%% And 2 container types;
-type array() :: tuple().
-type props() :: [pair()].
%%
%% where
-type pair() :: {str(), mason()}.
%%
%% The top type is
-type mason() :: str() | num() | boo() | nul() | props() | array() | pair().
%%

%% We map erlang types like this;
%%  map() -> props()
%%  tuple() -> array() | props()
%%  list() -> str() | array() | props().
%%  atom() -> str() | bool() | null()
%%  binary() -> str()
%%  integer() - num()
%%  float() - num()
%%  pid(), port(), ref(), fun() -> str()
%%

dbg(Z) -> io:fwrite(standard_error, "~n~p ~s:~w: ~p~n", Z),
          case hd(Z) of error -> error(tl(Z)); _ -> ok end.
-define(DBG(L, X), (fun(Z) -> dbg([L, ?FUNCTION_NAME, ?LINE, Z]), Z end)(X)).

file(File) ->
    case file:consult(File) of
        {ok, [T]} -> mason(T);
        Err -> error({file_error, File, Err})
    end.

mason(Bin) when is_binary(Bin) ->
    mason(binary_to_term(Bin));
mason(T) ->
    coerce(T).

%% type guards and introspecion

-define(IS_ARRAY(X), is_tuple(X)).
-define(IS_PROPS(X), (X==[] orelse ?IS_PAIR(hd(X)))).
-define(IS_PAIR(X), is_map(X)).
-define(IS_NUL(X), (X == null)).
-define(IS_BOO(X), is_boolean(X)).
-define(IS_NUM(X), is_number(X)).
-define(IS_STR(X), is_binary(X)).
type(X) when ?IS_PROPS(X) -> props;
type(X) when ?IS_ARRAY(X) -> array;
type(X) when ?IS_PAIR(X) -> pair;
type(X) when ?IS_BOO(X) -> bool;
type(X) when ?IS_NUL(X) -> null;
type(X) when ?IS_NUM(X) -> num;
type(X) when ?IS_STR(X) -> str;
type(X) -> ?DBG(error, {bad_type, X}).

-define(IS_PRIM(X), ?IS_BOO(X); ?IS_STR(X); ?IS_NUM(X); ?IS_NUL(X)).

%% When we walk the tree, we'll call our action fun
-type action_fun() :: fun((types(), mason()) -> action()).
%%
%% for each node in the graph. We'll pass the type;
-type types() :: props | array | pair | empty | str | num | lit.
%%
%% and the value, and the action must return;
-type action() :: keep | drop | {replace, mason()} | walk.
%%
%% The node will be kept, dropped, replaced, or recursed
%% over, depending on the action.

-spec walk(action_fun(), mason()) -> mason().
walk(F, Array) when ?IS_ARRAY(Array) ->
    case F(array, Array) of
        walk -> deep_walk(F, Array);
        keep -> Array;
        {replace, R} when ?IS_ARRAY(R) -> deep_walk(F, R);
        {replace, R} -> R;
        R -> error({bad_action, {walk_array, R}})
    end;
walk(F, Props) when ?IS_PROPS(Props) ->
    case F(props, Props) of
        walk -> deep_walk(F, Props);
        keep -> Props;
        {replace, R} when ?IS_PROPS(R) -> deep_walk(F, R);
        {replace, R} -> R;
        R -> error({bad_action, {walk_props, R}})
    end;
walk(F, Pair) when ?IS_PAIR(Pair) ->
    case F(pair, Pair) of
        drop -> drop;
        walk -> w_pair(F, Pair);
        keep -> Pair;
        {rename, K} -> pair(K, walk(F, val(Pair)));
        {rewrite, V} -> pair(key(Pair), walk(F, V));
        {replace, P} when ?IS_PAIR(P) -> walk(F, P);
        R -> error({bad_action, {walk_pair, R}})
    end;
walk(F, Val) when ?IS_PRIM(Val) ->
    case F(prim, Val) of
        keep -> Val;
        {replace, R} -> R;
        R -> error({bad_action, {walk_prim, R}})
    end.

w_pair(F, Pair) ->
    pair(key(Pair), walk(F, val(Pair))).

deep_walk(F, Es) when is_tuple(Es) ->
    to_tuple(deep_walk(F, to_list(Es)));
deep_walk(F, Es) when is_list(Es) ->
    lists:foldr(fun(E, A) -> deep_walker(F, E, A) end, [], Es).

deep_walker(F, E, A) ->
    case walk(F, E) of
        drop -> A;
        X -> [X|A]
    end.

%% Returns a term().
%% array() -> tuple(), props() => proplist(), str() -> atom().
unmason(X) when ?IS_ARRAY(X) ->
    list_to_tuple(lists:map(fun unmason/1, tuple_to_list(X)));
unmason(X) when ?IS_PROPS(X) ->
    lists:map(fun unmason/1, X);
unmason(X) when ?IS_PAIR(X) ->
    unmason(hd(maps:to_list(X)));
unmason(X) when ?IS_NUL(X) ->
    null;
unmason(X) when ?IS_BOO(X) ->
    X;
unmason(X) when ?IS_NUM(X) ->
    X;
unmason(X) when ?IS_STR(X) ->
    list_to_atom(binary_to_list(X)).

%% Returns a mason(). The empty list can be a proplist, a list, or
%% a string. we coerce it to array for no particularly good reason.

coerce(T) when is_tuple(T) ->
    coerce_tuple(T);
coerce(M) when is_map(M) ->
    coerce_map(M);
coerce(L) when is_list(L) ->
    coerce_list(L);
coerce(B) when is_boolean(B) ->
    B;
coerce(B) when is_binary(B)->
    B;
coerce(N) when is_number(N) ->
    N;
coerce(X) ->
    str(X).

-define(IS_CHAR(X), 0 =< X, X =< 255).
-define(C(N, X), ?IS_CHAR(element(N, X))).
-define(T(N, X), tuple_size(X) =:= N).
-define(IS_IPV4(X), ?T(4, X), ?C(1, X), ?C(2, X), ?C(3, X), ?C(4, X)).
-define(IS_ASCII(C), 16#20 =< C, C =< 16#7e). %% ascii
-define(IS_TPAIR(X), ?T(2, X), is_atom(element(1, X))).

%% masonify a tuple: to Array. Unless it looks like an ipv4 address.
coerce_tuple({A, B, C, D} = X) when ?IS_IPV4(X) ->
    to_bin([to_list(A), $., to_list(B), $., to_list(C), $., to_list(D)]);
coerce_tuple(T) ->
    to_tuple(lists:map(fun coerce/1, to_list(T))).

%% masonify a map: to Props.
coerce_map(M) ->
    maps:fold(fun(K, V, O) -> [pair(K, coerce(V))|O] end, [], M).

%% masonify a list: to Str, Props, or Array.
-spec coerce_list(list()) -> empty | str | props | array.
coerce_list(L) ->
    case L of
        [K|V] when (not is_list(V)) -> {coerce(K), coerce(V)};
        [P|_] when ?IS_TPAIR(P) -> coerce_list(props, L);
        [C|_] when ?IS_ASCII(C) -> coerce_list(str, L);
        _ -> to_tuple(lists:map(fun coerce/1, L))
    end.

%% masonify a list, given that the type is either T (where T is 'str'
%% or 'props') or 'array'. Return T if the checker accepts each
%% element, else return 'array'.
coerce_list(T, L) ->
    case {T, check_list(checker(T), L, [])} of
        {_, false} -> to_tuple(lists:map(fun coerce/1, L));
        {props, P} -> P;
        {str, S} -> str(S)
    end.

check_list(_, [], O) -> lists:reverse(O);
check_list(P, [E|R], O) ->
    case P(E) of
        false -> false;
        M -> check_list(P, R, [M|O])
    end.

checker(props) -> fun check_pair/1;
checker(str) -> fun check_char/1.

check_pair({K, V})  ->
    case coerce(K) of
        S when ?IS_STR(S) -> pair(S, coerce(V));
        _ -> false
    end;
check_pair(_) -> false.

check_char(C) when ?IS_ASCII(C) -> C;
check_char(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% generic

%% if X is a props()/array() with sizeof 1, return the element.
singlet({X}) when ?IS_ARRAY({X}) -> X;
singlet([Pair]) when ?IS_PAIR(Pair) -> Pair;
singlet(X) -> error({expected_singlet, type(X), X}).

%% concat props()/array(), uniquely
umerge(P1, P2) when ?IS_PROPS(P1), ?IS_PROPS(P2) ->
    lists:uniq(P1++P2);
umerge(A1, A2) when ?IS_ARRAY(A1), ?IS_ARRAY(A2) ->
    to_tuple(lists:uniq(to_list(A1)++to_list(A2))).

%% concat props()/array(), non-uniquely
merge(P1, P2) when ?IS_PROPS(P1), ?IS_PROPS(P2) ->
    P1++P2;
merge(A1, A2) when ?IS_ARRAY(A1), ?IS_ARRAY(A2) ->
    to_tuple(to_list(A1)++to_list(A2)).

%% compare an erlang term to a mason()
eq(Eterm, Cterm) ->
    coerce(Eterm) =:= Cterm.

member(V, L) ->
    lists:member(unclint(V), L).

all_is(F, P) when is_function(F, 1), ?IS_PROPS(P) ->
    lists:all(F, P);
all_is(F, A) when is_function(F, 1), ?IS_ARRAY(A) ->
    lists:all(F, to_list(A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fold helper
%% we allow array(), props, and regular list().

fold(F, array, X) ->
    array(fold(F, {}, X));
fold(F, props, X) ->
    props(lists:foldl(F, [], X));
fold(F, A, Array) when ?IS_ARRAY(Array) ->
    fold(F, A, to_list(Array));
fold(F, A, List) when is_list(List) ->
    lists:foldl(F, A, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicates

sizeof(P) when ?IS_PROPS(P) -> length(P);
sizeof(A) when ?IS_ARRAY(A) -> tuple_size(A);
sizeof(_) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% str() methods

str(X) ->
    to_bin(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% array() methods

array(X) ->
    to_tuple(X).

cons(E, A) when ?IS_ARRAY(A) ->
    case E of
        null -> A;
        _ -> to_tuple([E|to_list(A)])
    end.

decons(A) when ?IS_ARRAY(A) ->
    {head(A), tail(A)}.

head(A) when ?IS_ARRAY(A) ->
    [H|_] = to_list(A),
    H.

tail(A) when ?IS_ARRAY(A) ->
    [_|T] = to_list(A),
    to_tuple(T).

nth(N, A) when is_integer(N), ?IS_ARRAY(A) ->
    case N =< tuple_size(A) of
        true -> element(N, A);
        false -> error({array_too_short, N, A})
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% props() methods

%% constructor
props(Pair) when ?IS_PAIR(Pair) ->
    props([Pair]);
props(A) when ?IS_ARRAY(A)->
    props(lists:zip(lists:seq(0, sizeof(A)-1), tuple_to_list(A)));
props(List) when is_list(List) ->
    lists:foldr(fun check_pair/2, [], List).

check_pair(null, Props) -> Props;
check_pair(Pair, Props) when ?IS_PAIR(Pair) -> [Pair|Props];
check_pair({K, V}, Props) -> [pair(K, V)|Props];
check_pair(P, _) -> error({bad_pair, P}).

%% return the value of the pair with key Key from Pairs, or
%% Default. Fails unless Key is unique.
peek(Key, Pairs, Default) ->
    try peek(Key, Pairs)
    catch _:_ -> Default
    end.

peek(Key, Pairs) when ?IS_PROPS(Pairs) ->
    case match(Key, Pairs) of
        [] -> throw({key_not_found, {Key, Pairs}});
        [P] -> val(P);
        Dup -> throw({duplicate_key, Dup})
    end.

match(Key, Pairs) ->
    K = str(Key),
    lists:filter(fun(Pair) -> key(Pair) =:= K end, Pairs).

%% remove all pairs with key in Keys from Props.
rm(Keys, Props) when ?IS_PROPS(Props) ->
    {_, Ps} = take(Keys, Props),
    Ps.

%% split Props into {Props1, Props2}, where Props1 have keys in Keys.
take(Key, Pairs) when not is_list(Key) ->
    take([Key], Pairs);
take(Keys, Pairs) when ?IS_PROPS(Pairs) ->
    lists:foldr(fun taker/2, {[], Pairs}, Keys).

taker(Key, {Ps, Pairs}) ->
    K = str(Key),
    case lists:partition(fun(Pair) -> key(Pair) == K end, Pairs) of
        {[P], Rest} -> {[P|Ps], Rest};
        _ -> {Ps, Pairs}
    end.

%% select Pair with key Key, and mutate it by running Fun on its value.
mutate(Key, Fun, Ps) when not ?IS_STR(Key) ->
    mutate(str(Key), Fun, Ps);
mutate(Key, Fun, Ps) when ?IS_STR(Key), is_function(Fun), ?IS_PROPS(Ps) ->
    lists:map(mk_mutator(Key, Fun), Ps).

mk_mutator(Key, Fun) ->
    fun(Pair) -> mutator(Key, Fun, Pair) end.

mutator(Key, Fun, Pair) ->
    case key(Pair) =:= Key of
        true -> pair(Key, Fun(val(Pair)));
        false -> Pair
    end.

%% add pair() to props()
add(Pair, Pairs) when ?IS_PAIR(Pair), ?IS_PROPS(Pairs)  ->
    [Pair|Pairs].

keys(Props) when ?IS_PROPS(Props) ->
    [K || {K, _} <- kvs(Props)].

%% turn props() into list({key(), val()}).
kvs(Props) when ?IS_PROPS(Props) ->
    [kv(P) || P <- Props].

%% A subset of JSONpath, basically.
%% https://datatracker.ietf.org/doc/draft-ietf-jsonpath-base/
%%
%% Return a list of Leaf by searching in Props for Path. Path looks
%% like {k1, k2, [k3, k4], k5}; an array of K, where K is eiter a Key
%% or a list of Key. A Key is either a string (for props), an integer
%% (for array), or '*' (wildcard). A Leaf is {Val, Keys}.
path(Path, Props) when is_list(Path) ->
    path({Path}, Props);
path(Path, Props) when is_tuple(Path) ->
    mangle(path(path(Path), [[]], Props)).

%% mangle the output of path/5, list([Val, Kn,,, K0]), to the output
%% of path/2, list({Val, [K0,,, Kn]}).
mangle([]) -> [];
mangle([[Val|R]|Paths]) ->
    [{Val, lists:reverse(R)}|mangle(Paths)].

path(Path, Rs, Array) when ?IS_ARRAY(Array) ->
    path(Path, Rs, props(Array));
path(Path, Rs, Props) when ?IS_PROPS(Props) ->
    PP = path_peek(Path, keys(Props)),
    case is_list(PP) of
        true ->
            [SP++R || SP <- subpaths(Path, Props, PP), R <- Rs];
        false ->
            case path_elements_left(Path) of
                0 ->
                    Val = peek(PP, Props),
                    PE = path_element(PP),
                    [[Val, PE|R] || R <- Rs];
                _ ->
                    PE = path_element(PP),
                    path(path_next(Path), [[PE|R] || R <- Rs], peek(PP, Props))
            end
    end.

%% coerce path elements to str()/int()
path_element(PE) ->
    case {is_atom(PE), to_integer(PE)} of
        {true, false} -> str(PE);
        {false, I} when is_integer(I)-> I;
        _ -> PE
    end.

subpaths(Path, Props, SubpathHeads) ->
    Subpath = fun(K, Rs) -> subpath(Path, Props, K, Rs) end,
    lists:append(lists:foldr(Subpath, [], SubpathHeads)).

subpath(Path, Props, K, Rs) ->
    try [path(path_mutate(Path, K), [[]], Props)|Rs]
    catch throw:_ -> Rs
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper struct for Path

%% constructor
path(Path) ->
    #{ptr => 1,
      size => tuple_size(Path),
      path => Path}.

path_next(Path) ->
    maps:update_with(ptr, fun(V) -> V+1 end, Path).

path_elements_left(#{ptr := N, size := Size}) ->
    Size - N.

path_mutate(#{ptr := N} = Path, K) ->
    maps:update_with(path, fun(P) -> setelement(N, P, K) end, Path).

path_peek(#{ptr := N, path := Path}, Wildcard) ->
    case element(N, Path) of
        '*' -> Wildcard;
        Part -> Part
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% de/constructor for pair()

pair(K, V) ->
    try type(V)
    catch _:R -> error({pair_error, {R, {K, V}}})
    end,
    #{to_bin(K) => V}.

key(Pair) ->
    element(1, kv(Pair)).

val(Pair) ->
    element(2, kv(Pair)).

kv(Pair) ->
    [{K, V}] = to_list(Pair),
    {K, V}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prettyprint as json

json(X) ->
    #{out := O} = emit(X, #{out => [], stack => []}),
    lists:reverse(lists:flatten(O)).

emit(X, O) ->
    case type(X) of
        array -> emit_array(X, O);
        props -> emit_object(X, O);
        pair -> emit_pair(X, O);
        null -> emit_null(O);
        bool -> emit_bool(X, O);
        num -> emit_num(X, O);
        str -> emit_str(X, O)
    end.

emit_array(A, O) ->
    close_array(emit_items(A, open_array(A, O))).

emit_items(A, O) ->
    lists:foldl(fun emit_item/2, O, to_list(A)).

open_array(A, O) ->
    case calc_array_indent(A) of
        0 -> update("[", push(a0, O));
        1 -> update("[", push(a1, O));
        _ -> update("[", push(aL, O))
    end.

%% if array size is 0, or array size is 1 and the item is a prim, we
%% keep array on one line

calc_array_indent(A) ->
    lists:foldl(fun array_indent/2, 0, to_list(A)).

array_indent(Item, N) ->
    case type(Item) of
        array -> l;
        props -> l;
        _ when is_integer(N) -> N+1;
        _ -> l
    end.

close_array(O) ->
    case peek(O) of
        a0 -> update("]", pop(a0, O));
        a1 -> update("]", pop(a1, O));
        al -> update("]", pop(al, O)) %emit_ws(0, pop(al, O)))
    end.

emit_item(I, O) ->
    case peek(O) of
        a1 -> emit(I, O);
        aL -> emit(I, emit_ws(0, poke(al, O)));
        al -> emit(I, update(", ", emit_ws(-1, O)))
    end.

emit_object(X, O) ->
    close_object(emit_pairs(X, open_object(X, O))).

open_object(X, O) ->
    case calc_object_indent(X) of
        0 -> update("{", push(o0, O));
        1 -> update("{", push(o1, O));
        _ -> update("{", push(oL, O))
    end.

emit_pairs(X, O) ->
    lists:foldl(fun emit_pair/2, O, X).

%% if object size is 0, or object size is 1 and the val is a prim, we
%% keep object on one line

calc_object_indent(X) ->
    lists:foldl(fun object_indent/2, 0, X).

object_indent(Pair, N) ->
    case is_integer(N) andalso type(val(Pair)) of
        array -> l;
        props ->
            case sizeof(val(Pair)) of
                Sz when Sz < 2 andalso is_integer(N) -> N+1;
                _ -> l
            end;
        _ when is_integer(N) -> N+1;
        _ -> l
    end.

close_object(O) ->
    case peek(O) of
        o0 -> update("}", pop(o0, O));
        o1 -> update("}", pop(o1, O));
        ol -> update("}", pop(ol, O)) %% )emit_ws(0, pop(ol, O)))
    end.

emit_pair(P, O) ->
    {K, V} = kv(P),
    case peek(O) of
        o1 -> emit(V, emit_key(K, emit_spc(1, O)));
        oL -> emit(V, emit_key(K, emit_ws(0, poke(ol, O))));
        ol -> emit(V, emit_key(K, update(", ", emit_ws(-1, O))))
    end.

emit_key(K, O) ->
    update([$", to_list(K), $", $:, $ ], O).

emit_bool(true, O) ->
    update("true", O);
emit_bool(false, O) ->
    update("false", O).

emit_null(O) ->
    update("null", O).

emit_num(I, O) when is_integer(I) ->
    update(to_list(I), O);
emit_num(F, O) when is_float(F) ->
    update(to_list(F), O).

emit_str(S, O) ->
    update([$", to_list(S), $"], O).

%% calculate and emit whitespace
emit_ws(0, O = #{stack := Z}) ->
    ws(length(Z)*2, O);
emit_ws(N, O = #{stack := Z}) when N < 0 ->
    ws((length(Z)+N)*2, O);
emit_ws(N, O) ->
    ws(N, O).

ws(N, O) ->
    case on_boring_line(O) of
        true -> emit_spc(1, O);
        false -> emit_spc(N, nl(O))
    end.

emit_spc(N, O) ->
    update(lists:duplicate(N, $ ), O).

on_boring_line(#{out := Out}) ->
    nomatch =/= re:run(Out, "^[\{\[][ ,\{\[]*\n").

nl(O) ->
    update([10], O).

push(C, O = #{stack := Z}) ->
    O#{stack => [C|Z]}.

pop(C, O = #{stack := [C|Z]}) ->
    O#{stack => Z}.

peek(#{stack := []}) ->
    null;
peek(#{stack := [C|_]}) ->
    C.

poke(C, O = #{stack := [_|Z]}) ->
    O#{stack => [C|Z]}.

update(Str, O = #{out := Out}) ->
    O#{out => [lists:reverse(lists:flatten(Str))|Out]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% casting

to_integer(S) ->
    try list_to_integer(to_list(S))
    catch _:_ -> false
    end.

to_bin(X) when is_list(X) ->
    iolist_to_binary(X);
to_bin(X) ->
    list_to_binary(to_list(X)).

to_tuple(X) when is_tuple(X) -> X;
to_tuple(X) when is_list(X) -> list_to_tuple(X).

to_list(X) when is_list(X) -> X;
to_list(X) when is_tuple(X) -> tuple_to_list(X);
to_list(X) when is_map(X) -> maps:to_list(X);
to_list(X) when is_float(X) -> float_to_list(X, [{decimals, 32},compact]);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_reference(X) -> ref_to_list(X);
to_list(X) when is_pid(X) -> pid_to_list(X);
to_list(X) when is_port(X) -> port_to_list(X);
to_list(X) when is_function(X) -> fun_info_mfa(X).

fun_info_mfa(X) ->
    {M, F, A} = erlang:fun_info_mfa(X),
    [to_list(M), $:, to_list(F), $/, to_list(A)].
