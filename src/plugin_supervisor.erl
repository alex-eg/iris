-module(plugin_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/0]).
-export([start_plugin_process/4]).

start_link() ->
    {ok, _Pid} = supervisor:start_link(?MODULE, []).

init([]) ->
    lager:info("plugin_supervisor started and has pid ~p", [self()]),
    {ok, {
       {one_for_one, 1, 10},
       []
      }}.


start_plugin_process(Supervisor, Plugin, Config, Worker) ->
    {ok, _Pid} = supervisor:start_child(Supervisor,
                                        {Plugin,
                                         {Plugin, start_link, [Config, Worker]},
                                         transient,
                                         brutal_kill,
                                         worker,
                                         [Plugin]}).
