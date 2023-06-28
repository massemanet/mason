-module(mason_path).

-export(
   [assert/2,
    patch/2,
    select/2]).

%% mason types
-type mason_primitive() :: string() | binary() | number() | true | false | null.
-type mason_key() :: atom() | string() | binary().
-type mason_val() :: mason_primitive() | mason_term().
-type mason_assoc() :: #{mason_key() => mason_val()}.
-type mason_single():: mason_primitive() | mason_assoc().
-type mason_term() :: mason_single() | [mason_single()].

%% path expression types
-type px_kv() :: {mason_key(), mason_val()}.
-type px_selector() :: {px(), mason_primitive()}.
-type px_item() :: mason_key() | px_selector() | px_kv().
-type px() :: [px_item()].

%% type guards. they should ideally match the types.
-define(is_primitive(X), (?is_string(X) orelse ?is_jsony(X))).
-define(is_key(X), (is_atom(X) orelse ?is_string(X))).
-define(is_val(X), (?is_primitive(X) orelse ?is_term(X))).
-define(is_assoc(X), (is_map(X))).
-define(is_single(X), (?is_primitive(X) orelse ?is_assoc(X))).
-define(is_term(X), (?is_single(X) orelse (is_list(X) andalso ?is_single(hd(X))))).
-define(is_kv(X), (?is_t2_(X, ?is_key(?e1(X)), ?is_val(?e2(X))))).
-define(is_selector(X), (?is_t2_(X, ?is_pxy(?e1(X)), ?is_val(?e2(X))))).
-define(is_item(X), (?is_key(X) orelse ?is_selector(X) orelse ?is_kv(X))).
-define(is_px(X), (is_list(X) andalso ?is_item(hd(X)))).

%% helpers
-define(is_stringy(X), (is_list(X) andalso $\s =< hd(X) andalso hd(X) =< $~)).
-define(is_string(X), (is_binary(X) orelse ?is_stringy(X))).
-define(is_jsony(X), (is_number(X) orelse is_boolean(X) orelse (X =:= null))).
-define(is_pxy(X), (is_list(X) andalso ?is_itemy(hd(X)))).
-define(is_itemy(X), (?is_key(X) orelse ?is_itemy_tuple(X))).
-define(is_itemy_tuple(X), (?is_t2(X) andalso is_list(?e1(X)) andalso ?is_t2(hd(?e1(X))))).
-define(is_t2(X), (tuple_size(X) =:= 2)).
-define(is_t2_(X, E1, E2), (?is_t2(X) andalso (E1) andalso (E2))).
-define(e1(X), element(1, X)).
-define(e2(X), element(2, X)).

-spec assert(px_selector(), mason_term()) -> boolean().
assert({PX, Val}, M) when ?is_selector({PX, Val})->
    assert(PX, Val, M).

-spec assert(px(), mason_primitive(), mason_term()) -> boolean().
assert(PX, Val, M) when ?is_px(PX) andalso ?is_val(Val) andalso ?is_term(M) ->
    try select(PX, M) =:= Val
    catch _:_ -> false
    end.

-spec select(px(), mason_term()) -> mason_val().
%% M is an assoc
select([PXi], M) when ?is_key(PXi), ?is_assoc(M) ->
    maps:get(PXi, M);
select([PXi|PX], M) when ?is_key(PXi), ?is_assoc(M) ->
    select(PX, maps:get(PXi, M));
select([PXi|_], M) when ?is_selector(PXi), ?is_assoc(M) ->
    error({select_error, {selector_on_map, PXi, M}});

%% M is a list
select([PXi], M) when ?is_selector(PXi), is_list(M) ->
    [X || X <- M, assert(PXi, X)];
select([PXi|PX], M) when ?is_selector(PXi), is_list(M) ->
    [select(PX, X) || X <- M, assert(PXi, X)];
select([PXi], M) when ?is_key(PXi), is_list(M) ->
    [V || #{PXi := V} <- M];
select([PXi|_], M) when ?is_key(PXi), is_list(M) ->
    error({select_error, {key_on_list, PXi, hd(M)}});

%% badarg
select(PX, M) ->
    error({select_error, {badarg, PX, M}}).

-spec patch(px(), mason_term()) -> mason_term().
%% M is an assoc
patch([K], M) when ?is_key(K), ?is_assoc(M) ->
    error({patch_error, {key_on_assoc, K}});
patch([K|PX], M) when ?is_key(K), ?is_assoc(M) ->
    M#{K => patch(PX, maps:get(K, M))};
patch([{K, V}], M) when ?is_kv({K, V}), ?is_assoc(M) ->
    M#{K => V};
patch([{K, V}|_], M) when ?is_kv({K, V}), ?is_assoc(M) ->
    error({patch_error, {kv_on_assoc, {K, V}}});
patch([S|_], M) when ?is_selector(S), ?is_assoc(M) ->
    error({patch_error, {select_on_assoc, S}});
patch([A|_], M) when ?is_assoc(A), ?is_assoc(M) ->
    error({patch_error, {assoc_on_assoc, A}});

%% M is a list
patch([K|_], M) when ?is_key(K), is_list(M) ->
    error({patch_error, {key_on_list, K, hd(M)}});
patch([KV|_], M) when ?is_kv(KV), is_list(M) ->
    error({patch_error, {kv_on_list, KV, hd(M)}});
patch([S], M) when ?is_selector(S), is_list(M) ->
    error({patch_error, {selector_on_list, S, hd(M)}});
patch([S|PX], M) when ?is_selector(S), is_list(M) ->
    lists:map(fun(E) -> patcher(S, PX, E) end, M);
patch([A|_], M) when ?is_assoc(A), is_list(M) ->
    error({patch_error, {assoc_on_list, A}});
patch([A], M) when ?is_assoc(A) , is_list(M) ->
    [A|M];

%% badarg
patch(PX, M) ->
    error({patch_error, {badarg, PX, M}}).

patcher(Selector, PX, M) ->
    case assert(Selector, M) of
        true -> patch(PX, M);
        false -> M
    end.
