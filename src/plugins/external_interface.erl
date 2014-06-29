-module(external_interface).
%%-behavior(gen_server).
-behavior(iris_plugin).
-export([start/3, process_message/2]).
-export([start_link/2, accept_loop/4]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state,
        {parent_worker,
         port,
         socket,
         target_jid
        }).

start(Supervisor, WorkerConfig, From) ->
    {ok, _Pid} = supervisor:start_child(Supervisor,
                                        {external_interface,
                                         {?MODULE, start_link, [WorkerConfig, From]},
                                         transient,
                                         5000,
                                         worker,
                                         [?MODULE]}).

start_link(WorkerConfig, From) ->
    Config = proplists:get_value(external_interface, jid_config:other_config(WorkerConfig)),
    Target = proplists:get_value(target_jid, Config),
    Port =  proplists:get_value(port, Config),
    State = #state{parent_worker = From,
                   port = Port,
                   target_jid = Target
                  },
    gen_server:start_link({local, external_interface}, ?MODULE, State, []).

init(State) ->
    Port = State#state.port,
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    NewState = State#state{socket = LSocket},
    gen_server:cast(self(), wait_for_connection),
    {ok, NewState}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(wait_for_connection, State) ->
     proc_lib:spawn(?MODULE, accept_loop,
                    [self(),
                     State#state.socket,
                     State#state.parent_worker,
                     State#state.target_jid]),
    {noreply, State};
handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ulog:info("external_interface", "server terminated. Reason: ~p", [Reason]),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

accept_loop(Parent, LSocket, Subscriber, Recepient) ->
    ulog:debug("accept_loop", "started successfully!", []),
    {ok, Socket} = gen_tcp:accept(LSocket),
    loop(Socket, Subscriber, Recepient),
    gen_server:cast(Parent, wait_for_connection),
    exit(self(), shutdown).

loop(Socket, Subscriber, Recepient) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ulog:debug("external_interface", "recieved data via socket: ~p", [Data]),
            Message = binary_to_list(Data),
            gen_server:cast(Subscriber, {send_message, Message, Recepient}),
            loop(Socket, Subscriber, Recepient);
        {error, closed} ->
            ulog:debug("external_interface", "shutting down...", ""),
            ok
    end.

process_message(_Msg, _Cfg) ->
    nope.

