-module(mason_recs).

-behaviou(gen_server).

%% API
-export(
   [learn/1,
    keys/3]).

%% gen_server callbacks
-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

%% `Arity' is the record arity, 1 less than the tuple arity
keys(Rec, Arity, Mod) ->
    lookup({Rec, Arity, Mod}).

learn(Mod) when is_atom(Mod) ->
    learn([Mod]);
learn(Mods) when is_list(Mods) ->
    populate(Mods).

init(_) ->
    table(),
    learn(modules()),
    {ok, state}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

modules() ->
    case application:get_env(mason, records) of
        undefined -> [];
        {ok, Modules} -> Modules
    end.

populate(Modules) ->
    lists:foreach(fun module/1, Modules).

module(Module) ->
    case code:which(Module) of
        Filename when is_list(Filename) -> file(Filename);
        _ -> ok
    end.

-define(DBGINFO(AST), {debug_info_v1, erl_abstract_code, {AST, _}}).
file(File) ->
    case beam_lib:chunks(File, [debug_info]) of
        {ok, {Mod, [{debug_info, ?DBGINFO(AST)}]}} -> mod(Mod, AST);
        _ -> []
    end.

mod(Mod, AST) ->
    lists:foreach(fun(F) -> form(Mod, F) end, AST).

-define(REC(Name, Fields), {attribute, _, record, {Rec, Fields}}).
form(Mod, Form) ->
    case Form of
        ?REC(Rec, Fields) -> fields(Mod, Rec, Fields);
        _ -> ok
    end.


fields(Mod, Rec, Fields) ->
    case length(Fields) of
        Arity when 0 < Arity -> store({{Rec, Arity, Mod}, field_names(Fields)});
        _ -> ok
    end.

field_names(Fields) ->
    [field_name(F) || F <- Fields].

field_name(Field) ->
    case Field of
        {record_field, _, {atom, _, Name}} -> Name;
        {record_field, _, {atom, _, Name}, _} -> Name;
        {typed_record_field, F, _} -> field_name(F)
    end.

store(T) ->
    ets:insert(mason_recs, T).

lookup(Key) ->
    try ets:match(mason_recs, {Key, '$1'}) of
        [] -> [];
        [_, _|_] -> [];
        [[Val]] -> Val
    catch
        _:_ -> []
    end.

table() ->
    ets:new(mason_recs, [ordered_set, named_table, public]).
