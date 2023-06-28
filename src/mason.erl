-module(mason).

-export(
   [start_link/1]).

-export(
   [encode/1, encode/2,
    decode/1, decode/2]).

-export(
   [ts/2,
    record_learn/1, record_keys/2, record_keys/3,
    get_opt/2]).

%% mason types
-type options() :: map().
-type key() :: atom() | string() | binary().
-type primitive() :: string() | binary() | atom() | number() | true | false | null.
-type item() :: array() | object() | primitive().
-type object() :: #{key() => item()} | proplist:proplist().
-type array() :: [item()] | {item()}.
-type decoded() :: item().
-type encoded() :: string() | binary() | atom().

%% Type guards. They should ideally match the types.
%% Alas, there are two problems;
%% * the real types are recursive
%% * guards are pretty limited

%% abstractions
-define(e1(X), element(1, X)).
-define(e2(X), element(2, X)).
-define(is_t2(X), (tuple_size(X) =:= 2)).
-define(is_t2_(X, E1, E2), (?is_t2(X) andalso (E1) andalso (E2))).
-define(is_list_(X, E), (is_list(X) andalso ((length(X) =:= 0) orelse (E)))).

%% helpers
-define(is_jsony(X), (is_number(X) orelse (is_boolean(X)) orelse (X =:= null))).
-define(is_string(X), (?is_list_(X, ($\s =< hd(X)) andalso (hd(X) =< $~)))).
-define(is_text(X), (?is_string(X) orelse (is_binary(X))) orelse (is_atom(X))).

-define(is_primitive(X), (?is_text(X) orelse ?is_jsony(X))).
-define(is_item(X), (is_list(X) orelse (is_tuple(X)) orelse (is_map(X)) orelse ?is_primitive(X))).
-define(is_prop(X), (?is_t2_(X, ?is_text(?e1(X)), ?is_item(?e2(X))))).
-define(is_proplist(X), (?is_list_(X, ?is_prop(hd(X))))).
-define(is_object(X), (is_map(X) orelse ?is_proplist(X))).
-define(is_array(X), (is_tuple(X) orelse ?is_list_(X, ?is_item(hd(X))))).
-define(is_decoded(X), (?is_array(X) orelse ?is_object(X) orelse ?is_primitive(X))).
-define(is_encoded(X), (?is_string(X) orelse is_binary(X) orelse is_atom(X))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

start_link(A) ->
    gen_server:start_link({local, mason}, mason_recs, A, []).

ts(TS, Unit) ->
    mason_encoder:ts(TS, Unit).

record_learn(M) ->
    mason_recs:learn(M).

record_keys(Name, Arity) ->
    record_keys(Name, Arity, '_').

record_keys(Name, Arity, Mod) ->
    mason_recs:keys(Name, Arity, Mod).

encode(X) ->
    encode(X, #{}).

-spec encode(term(), options()) -> encoded().
encode(X, Opts) when is_map(Opts) ->
    store_opts(Opts),
    try lists:flatten(mason_encoder:emit(X))
    after delete_opts(Opts)
    end.

decode(JSON) ->
    decode(JSON, #{}).

-spec decode(encoded(), options()) -> decoded().
decode(JSON, Opts) when ?is_encoded(JSON), is_map(Opts) ->
    store_opts(Opts),
    try lift(mason_parser:parse(lift(mason_lexer:string(to_str(JSON)))))
    catch throw:Err -> Err
    after delete_opts(Opts)
    end.

to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_list(X) -> X.

lift({ok, Val, _}) -> Val;
lift({ok, Val}) -> Val;
lift({error, {Line, mason_lexer, Err}, _}) -> throw({error, {lexer, Line, Err}});
lift({error, {Line, mason_parser, Err}}) -> throw({error, {syntax, Line, lists:flatten(Err)}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options. since the parser/lexer don't allow us to pass options, we
%% resort to using the process dictionary

store_opts(Opts) ->
    maps:map(fun(K, V) -> put({mason, K}, V) end, Opts).

delete_opts(Opts) ->
    maps:map(fun(K, _) -> erase({mason, K}) end, Opts).

get_opt(K, Default) ->
    case get({mason, K}) of
        undefined -> Default;
        Val -> Val
    end.
