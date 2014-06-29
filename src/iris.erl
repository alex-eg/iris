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
    ulog:info("|            .doodxdol;.   'Ok.   ;X.             |"),
    ulog:info("|             OO::::lkXWO.;Xo      lK             |"),
    ulog:info("|            .Xc       ,WXXc       ON             |"),
    ulog:info("|            kN'  dxkk: KXN.      oNc             |"),
    ulog:info("|           .NN' KK0lcxOk0X.    .kklcodoc,        |"),
    ulog:info("|            xWk:0O,    ;OX;,..ddxKOl''cd00:      |"),
    ulog:info("|             ,oxOOx.    ;0OoKKc0K;  ..   'xKd;'. |"),
    ulog:info("|          .';cc:,.,oc    xOl;,..oxckOk.    .NXc  |"),
    ulog:info("|       ,l0d'.....,k0k0oclxo    'dOdl;;,    xK.   |"),
    ulog:info("|    .xOo:.        ;Okc  .OKkdk0KK'       ,xK.    |"),
    ulog:info("|   '0l.       'cx0Nl    'OkddKKKKdolc:;oXNo      |"),
    ulog:info("|     :OKOddkKKOKXXKc'..lKkKXd,  .:oOkdoc'        |"),
    ulog:info("|        .,:;,xNNOdNX00KOOX.dk       ;K;          |"),
    ulog:info("|            ;XX. kKOkO xNN, Od       kN,         |"),
    ulog:info("|            .XXl .xOk;.NNN0 .X;      'Nx         |"),
    ulog:info("|             :NXk. .. 'XXX0  ;Xc      Xo         |"),
    ulog:info("|              ,KNk.,lONWXo    .dKoolllN.         |"),
    ulog:info("|                ONNXko:.         .....'          |"),
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
       {one_for_one, 10, 60},
       [{core,
         {core, start_link, [self()]},
         transient,
         10000,
         worker,
         [core]}]
     }
    }.
