-module(external_interface).
%%-behavior(gen_server).
-behavior(iris_plugin).
-export([start/3, process_message/2]).
-export([shortcut/3, accept_loop/3]).
%%-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state,
        {supervisor,
         parent_worker,
         port,
         socket,
         target_jid
        }).

start(Supervisor, WorkerConfig, From) ->
    {ok, _Pid} = supervisor:start_child(Supervisor,
                                        {external_interface,
                                         {?MODULE, shortcut, [Supervisor, WorkerConfig, From]},
                                         transient,
                                         5000,
                                         worker,
                                         [?MODULE]}).

shortcut(Supervisor, WorkerConfig, From) ->
    Config = jid_config:other_config(WorkerConfig),
    Target = proplists:get_value(target_jid, Config),
    Port =  proplists:get_value(port, Config),
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    proc_lib:spawn(?MODULE, accept_loop, [LSocket, From, Target]).

accept_loop(LSocket, Subscriber, Recepient) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    loop(Socket, Subscriber, Recepient).

loop(Socket, Subscriber, Recepient) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ulog:debug("external_interface", "recieved data via socket: ~p", [Data]),
            Message = binary_to_list(Data),
            gen_server:cast(Subscriber, {send_message, binary_to_list(Data), Recepient}),
            loop(Socket, Subscriber, Recepient);
        {error, closed} ->
            ok
    end.

%% start(Supervisor, WorkerConfig, From) ->
%%     {ok, _Pid} = supervisor:start_child(Supervisor,
%%                                         {external_interface,
%%                                          {?MODULE, start_link, [Supervisor, WorkerConfig, From]},
%%                                          transient,
%%                                          5000,
%%                                          worker,
%%                                          [?MODULE]}).
%% start_link(Supervisor, JidConfig, From) ->
%%     Config = proplists:get_value(external_interface, jid_config:other_config(JidConfig)),
%%     State = #state{supervisor = Supervisor,
%%                    parent_worker = From,
%%                    port = proplists:get_value(port, Config),
%%                    target_jid = proplists:get_value(target_jid, Config)
%%                   },
%%     gen_server:start_link({local, external_interface}, ?MODULE, State, []).

%% init(State) ->
%%     Port = State#state.port,
%%     {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
%%     NewState = State#state{socket = LSocket},
%%     proc_lib:spawn(?MODULE, accept_loop, [self(), LSocket, State#state.parent_worker])
%%     {ok, NewState}.

%% handle_call(Any, _From, State) ->
%%     ulog:debug("external_interface", "Recieved unknown request: ~p", [Any]),
%%     {noreply, State}.

%% handle_cast(accepted, State) ->
%%     ulog:debug("external_interface", "got connection", []),
    
%% handle_cast(Any, State) ->
%%     ulog:debug("external_interface", "Recieved unknown cast: '~p'", [Any]),
%%     {noreply, State}.

%% handle_info(Msg, State) -> 
%%     ulog:debug("external_interface", "Recieved unknown message: '~p'", [Msg]),
%%     {noreply, State}.

%% terminate(Reason, _State) ->
%%     ulog:info("External interface server terminated. Reason: ~p", [Reason]),
%%     ok.

%% code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% %% gen_server callbacks end

%% accept_loop(Server, LSocket, Subscriber) ->
%%     {ok, Socket} = gen_tcp:accept(LSocket),
%%     gen_server:cast(Server, {accepted, self(), Subscriber}),
%%     loop(Socket, Subscriber),
%%     {normal, shutdown}.

%% loop(Socket, Subscriber, Recepient) ->
%%     case gen_tcp:recv(Socket, 0) of
%%         {ok, Data} ->
%%             ulog:debug("external_interface", "recieved data via socket: ~p", [Data]),
%%             Message = binary_to_list(Data),
%%             gen_server:cast(Subscriber, {send_message, binary_to_list(Data), Recepient}),
%%             loop(Socket, Subscriber, Recepient);
%%         {error, closed} ->
%%             ok
%%     end.

%% handle_cast({new_post, Data}, State = #state{config = Config}) ->
%%     Reciever = list_to_binary("maintenance@conference.anoma.ch"),
%%     Sender = format_str("~s", [Config#jid_info.jid]),
%%     Packet1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, Data),
%%     Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
%%     Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Reciever),
%%     gen_server:cast(self(), {send_packet, Packet3}),
%%     {noreply, State};

process_message(_Msg, _Cfg) ->
    nope.

