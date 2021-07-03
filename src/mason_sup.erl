-module(mason_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(A) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, A).

init(A) ->
    {ok, {#{strategy => one_for_one}, [child(mason, A)]}}.

child(Mod, A) ->
    #{id => Mod, start => {Mod, start_link, [A]}}.
