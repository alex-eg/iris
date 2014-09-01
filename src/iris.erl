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
    %% lager:info("#-------------=======================-------------#"),
    %% lager:info("|---------======== IRIS starting ========---------|"),
    %% lager:info("#-------------=======================-------------#"),
    %% lager:info("|                                                 |"),
    %% lager:info("|                               .',               |"),
    %% lager:info("|                            'odolX'              |"),
    %% lager:info("|            .doodxdol;.   'Ox.   ;X.             |"),
    %% lager:info("|             OO::::lxXWO.;Xo      lX             |"),
    %% lager:info("|            .Xc       ,WXXc       ON             |"),
    %% lager:info("|            xN'  dxxx: XXN.      oNc             |"),
    %% lager:info("|           .NN' XX0lcxOx0X.    .xxlcodoc,        |"),
    %% lager:info("|            xWx:0O,    ;OX;,..ddxXOl''cd00:      |"),
    %% lager:info("|             ,oxOOx.    ;0OoXXc0X;  ..   'xXd;'. |"),
    %% lager:info("|          .';cc:,.,oc    xOl;,..oxcxOx.    .NXc  |"),
    %% lager:info("|       ,l0d'.....,x0x0oclxo    'dOdl;;,    xX.   |"),
    %% lager:info("|    .xOo:.        ;Oxc  .OXxdx0XX'       ,xX.    |"),
    %% lager:info("|   '0l.       'cx0Nl    'OxddXXXXdolc:;oXNo      |"),
    %% lager:info("|     :OXOddxXXOXXXXc'..lXxXXd,  .:oOxdoc'        |"),
    %% lager:info("|        .,:;,xNNOdNX00XOOX.dx       ;X;          |"),
    %% lager:info("|            ;XX. xXOxO xNN, Od       xN,         |"),
    %% lager:info("|            .XXl .xOx;.NNN0 .X;      'Nx         |"),
    %% lager:info("|             :NXx. .. 'XXX0  ;Xc      Xo         |"),
    %% lager:info("|              ,XNx.,lONWXo    .dXoolllN.         |"),
    %% lager:info("|                ONNXxo:.         .....'          |"),
    %% lager:info("|                .x.                              |"),
    %% lager:info("|                                                 |"),
    %% lager:info("#-------------=======================-------------#"),

    supervisor:start_link({local, iris_main_sup}, ?MODULE, []).

stop(_State) ->
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
