-module(mason_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, Args) ->
    mason_sup:start_link(Args).

stop(_State) ->
    ok.
