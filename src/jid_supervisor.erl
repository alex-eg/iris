-module(jid_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/2]).

start_link(Config, Name) ->
    {ok, _Pid} = supervisor:start_link({local, Name}, ?MODULE, [Config]).

init([Config]) ->
    WorkerName = list_to_atom(jid_config:jid(Config)),
    ulog:debug("starting jid_supervisor for ~p, process is ~p", [WorkerName, self()]),
    {ok, {
       {one_for_all, 1, 10},
       [{WorkerName,
         {jid_worker, start_link, [Config, WorkerName, self()]},
         transient,
         10000,
         worker,
         [jid_worker]}]
      }}.
