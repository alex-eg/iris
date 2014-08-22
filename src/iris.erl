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
    ulog:info("#-------------=======================-------------#"),
    ulog:info("|---------======== IRIS starting ========---------|"),
    ulog:info("#-------------=======================-------------#"),
    ulog:info("|                                                 |"),
    ulog:info("|                               .',               |"),
    ulog:info("|                            'odolX'              |"),
    ulog:info("|            .doodxdol;.   'Ox.   ;X.             |"),
    ulog:info("|             OO::::lxXWO.;Xo      lX             |"),
    ulog:info("|            .Xc       ,WXXc       ON             |"),
    ulog:info("|            xN'  dxxx: XXN.      oNc             |"),
    ulog:info("|           .NN' XX0lcxOx0X.    .xxlcodoc,        |"),
    ulog:info("|            xWx:0O,    ;OX;,..ddxXOl''cd00:      |"),
    ulog:info("|             ,oxOOx.    ;0OoXXc0X;  ..   'xXd;'. |"),
    ulog:info("|          .';cc:,.,oc    xOl;,..oxcxOx.    .NXc  |"),
    ulog:info("|       ,l0d'.....,x0x0oclxo    'dOdl;;,    xX.   |"),
    ulog:info("|    .xOo:.        ;Oxc  .OXxdx0XX'       ,xX.    |"),
    ulog:info("|   '0l.       'cx0Nl    'OxddXXXXdolc:;oXNo      |"),
    ulog:info("|     :OXOddxXXOXXXXc'..lXxXXd,  .:oOxdoc'        |"),
    ulog:info("|        .,:;,xNNOdNX00XOOX.dx       ;X;          |"),
    ulog:info("|            ;XX. xXOxO xNN, Od       xN,         |"),
    ulog:info("|            .XXl .xOx;.NNN0 .X;      'Nx         |"),
    ulog:info("|             :NXx. .. 'XXX0  ;Xc      Xo         |"),
    ulog:info("|              ,XNx.,lONWXo    .dXoolllN.         |"),
    ulog:info("|                ONNXxo:.         .....'          |"),
    ulog:info("|                .x.                              |"),
    ulog:info("|                                                 |"),
    ulog:info("#-------------=======================-------------#"),

    supervisor:start_link({local, main_sup}, ?MODULE, []).

stop(_State) ->
    exit(main_sup, shutdown),
    ok.

%% Supervisor behavior callback
init(_Args) ->
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
