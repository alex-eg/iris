-module(external_interface).
-behavior(gen_server).
-export([start/1, start_link/1, accept_loop/3]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state,
        {supervisor,
         port = 21285,
         socket
        }).

start(Supervisor) ->
    {ok, _Pid} = supervisor:start_child(Supervisor,
                                        {external_interface,
                                         {?MODULE, start_link, [Supervisor]},
                                         transient,
                                         5000,
                                         worker,
                                         [?MODULE]}).
start_link(Supervisor) ->
    State = #state{supervisor = Supervisor},
    gen_server:start_link({local, external_interface}, ?MODULE, State, []).

init(State) ->
    Port = State#state.port,
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    NewState = State#state{socket = LSocket},
    {ok, NewState}.

handle_call(Any, _From, State) ->
    ulog:info("Recieved UNKNOWN request: ~p", [Any]),
    {noreply, State}.

handle_cast({accepted, _Pid, Subscriber}, State = #state{socket = LSocket}) ->
    proc_lib:spawn(?MODULE, accept_loop, [self(), LSocket, Subscriber]),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved UNKNOWN cast: '~p'", [Any]),
    {noreply, State}.

handle_info(Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    ulog:info("External interface server terminated. Reason: ~p", [Reason]),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

accept_loop(Server, LSocket, Subscriber) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self(), Subscriber}),
    loop(Socket, Subscriber),
    {normal, shutdown}.

loop(Socket, Subscriber) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ulog:info("Recieved data via socket: ~p", [Data]),
            gen_server:cast(Subscriber, {socket_recieved, Data}),
            loop(Socket, Subscriber);
        {error, closed} ->
            ok
    end.
