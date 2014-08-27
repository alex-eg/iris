-module(plugin_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/1]).

start_link(Plugins) ->
    {ok, _Pid} = supervisor:start_link(?MODULE, Plugins).

init(Plugins) ->
    lager:info("plugin_supervisor started and has pid ~p", [self()]),
    {ok, {
       {one_for_one, 1, 10},
       [{Plugin,
         {Plugin, start, [Config, JidWorker]},
         transient,
         brutal_kill,
         worker,
         [Plugin]} || {Plugin, Config, JidWorker} <- Plugins]
      }}.
