-module(jid_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/2]).

start_link(Config, Name) ->
    %case supervisor:start_link({local, Name}, ?MODULE, [Config]) of
    %    {ok, _Pid} -> {ok, _Pid};
    %    {error, Error} -> ulog:error("start_link returned ~p", [Error])
    %end.
    {ok, _Pid} = supervisor:start_link({local, Name}, ?MODULE, [Config]).

init([Config]) ->
    WorkerName = list_to_atom(jid_config:jid(Config)),
    ulog:debug("starting jid_supervisor for ~p, process is ~p", [WorkerName, self()]),
    {ok, {
       {one_for_all, 1, 10},
       [{WorkerName,
         {jid_worker, start_link, [Config, WorkerName, self()]},
         transient,
         brutal_kill,
         worker,
         [jid_worker]}]
      }}.
