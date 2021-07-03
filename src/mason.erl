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

encode(X, Opts) ->
    store_opts(Opts),
    try lists:flatten(mason_encoder:emit(X))
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

lift({ok, Val, _}) -> Val;
lift({ok, Val}) -> Val;
lift({error, {Line, mason_lexer, Err}, _}) -> throw({error, {lexer, Line, Err}});
lift({error, {Line, mason_parser, Err}}) -> throw({error, {syntax, Line, lists:flatten(Err)}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options. since the parser/lexer don't allow us to pass options, use
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
