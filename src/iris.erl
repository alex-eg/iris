-module(iris).
-behavior(supervisor).
-behavior(application).
-export([start_shortcut/0]).
-export([start/2, stop/1]).
-export([init/1]).

-include("xmpp.hrl").

%% Cheater's shortcut
start_shortcut() ->
    ulog:info("Invoked shortcut start, pid is ~p", [self()]),
    {ok, Pid} = supervisor:start_link({local, main_sup}, ?MODULE, []),
    erlang:unlink(Pid).

%% Application behavior callbacks
start(normal, _StartArgs) ->
    ulog:info("------====== IRIS starting ======------"),
    {ok, _Pid} = supervisor:start_link({local, main_sup}, ?MODULE, []),
    {ok, self()}.

stop(_State) ->
    exit(main_sup, shutdown),
    ok.

%% Supervisor behavior callback
init(_Args) ->
    {ok, 
     {
       {one_for_one, 2, 60},
       [{root,
	 {root, start_link, [self()]},
	 transient, 
	 10000,
	 worker,
	 [root]}]
     }
    }.
