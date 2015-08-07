-module(iris).
-behavior(supervisor).
-behavior(application).
-export([start/0]).
-export([start/2, stop/1]).
-export([init/1]).

-include("xmpp.hrl").

%% Cheater's shortcut
start() ->
    application:ensure_all_started(iris).

%% Application behavior callbacks
start(normal, _StartArgs) ->
    lager:info("#-------------=======================-------------#"),
    lager:info("|---------======== IRIS starting ========---------|"),
    lager:info("#-------------=======================-------------#"),

    supervisor:start_link({local, iris_main_sup}, ?MODULE, []).

stop(_State) ->
    lager:info("---------=========== Stopping ===========----------"),
    exit(main_sup, shutdown),
    ok.

%% Supervisor behavior callback
init(_Args) ->
    lager:info("Main supervisor started and has pid ~p", [self()]),
    {ok,
     {
       {one_for_one, 1, 10},
       [{core,
         {core, start_link, [self()]},
         transient,
         10000,
         worker,
         [core]}]
     }
    }.
