-module(jid_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/2]).

start_link(Config, WorkerName) ->
    {ok, _Pid} = supervisor:start_link(?MODULE, [Config, WorkerName]).

init([Config, WorkerName]) ->
    lager:info("started and has pid ~p", [self()]),
    {ok, {
       {rest_for_one, 1, 10},
       [{WorkerName,
         {jid_worker, start_link, [Config, WorkerName, self()]},
         transient,
         2000,
         worker,
         [jid_worker]},
        {list_to_atom(atom_to_list(WorkerName) ++ "_plugin_supervisor"),
         {plugin_supervisor, start_link, []},
         transient,
         3000,
         supervisor,
         [plugin_supervisor]}]
      }}.
