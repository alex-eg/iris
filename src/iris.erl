-module(iris).
-behavior(supervisor).
-behavior(application).
-export([start_shortcut/0]).
-export([start/2, stop/1]).
-export([init/1]).

-include("xmpp.hrl").

%% Cheater's shortcut
start_shortcut() ->
    application:start(iris).

%% Application behavior callbacks
start(normal, _StartArgs) ->
    ulog:info("#---------===================---------#"),
    ulog:info("|-----====== IRIS starting ======-----|"),
    ulog:info("#---------===================---------#"),
    {ok, _Pid} = supervisor:start_link({local, main_sup}, ?MODULE, []),
    {ok, self()}.

stop(_State) ->
    exit(main_sup, shutdown),
    ok.

%% Supervisor behavior callback
init(_Args) ->
    {ok, 
     {
       {one_for_one, 10, 60},
       [{core,
         {core, start_link, [self()]},
         transient,
         10000,
         worker,
         [core]}]
     }
    }.
