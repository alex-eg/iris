-module(iris).
-behavior(supervisor).
-behavior(application).
-export([start/2, stop/1]).
-export([init/1]).

-include("xmpp.hrl").

start(_StartType, _StartArgs) ->
    supervisor:start({local, supervisor}, ?MODULE, [], []).

stop(_State) ->
    ok.

init(_Args) ->
    ulog:info("Main process and supervisor started and has PID ~p", [self()]),
    {ok, 
     {{one_for_one, 1, 60},
      [{root,
	{root, start, []},
	permanent, brutal_kill, supervisor, [root]}
      ]
     }
    }.
