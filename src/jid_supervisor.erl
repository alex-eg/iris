-module(jid_supervisor).
-behavior(supervisor).
-export([init/1]).
-export([start_link/2]).

start_link(Config, Name) ->
    {ok, Pid} = supervisor:start_link({local, Name}, ?MODULE, [Config, Name]),
    ulog:info("jid_supervisor ~p started and has pid ~p", [Name, Pid]),
    {ok, Pid}.

init([Config, Name]) ->
    SupervisorName = list_to_atom(atom_to_list(Name) ++ "_supervisor"),
    {ok, {
       {one_for_all, 1, 10},
       [{SupervisorName,
         {jid_worker, start_link, [Config, Name, self()]},
         transient,
         10000,
         worker,
         [jid_worker]}]
      }}.
