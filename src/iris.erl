-module(iris).
-behavior(supervisor).
-export([start/0]).
-export([init/1]).

-include("xmpp.hrl").

start() ->
    supervisor:start({local, supervisor}, ?MODULE, [], []).

init(_Args) ->
    ulog:info("Main process and supervisor started and has PID ~p", [self()]),
    {ok, 
     {{one_for_one, 1, 60},
	  [{root, 
	    {root, start, []},
	    permanent, brutal_kill, supervisor, [root]},
	   {worker1, start, []},
	    permanent, brutal_kill, supervisor, [root]}
     }}.
