-module(jid_worker).
-behavior(gen_server).
%% entry point
-export([start_link/3]).
%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% module API
-export([reply/2]).
-export([store_config/1, get_config/1,
         store_config/2, get_config/2,
         store_config/3, get_config/3]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

-record(state,
        {name,
         supervisor,
         session,
         plugin_supervisor,
         config_ets
        }).

start_link(Config, Name, Supervisor) ->
    ConfigEts = ets:new(config, [bag]),
    State = #state{name = Name,
                   supervisor = Supervisor,
                   config_ets = ConfigEts
                  },
    lists:foreach(fun(Entry) ->
                          lager:debug("Inserting ~w in worker config ets", [Entry]),
                          ets:insert(ConfigEts, Entry)
                  end,
                  Config),
    {ok, _Pid} = gen_server:start_link(?MODULE, State, []).

% gen_server callbacks

init(State) ->
    lager:info("Jid worker ~p started and has pid ~p", [State#state.name, self()]),
    Config = State#state.config_ets,
    Session = exmpp_session:start({1, 0}), %% retardation needed to start SSL authorization (designates xmpp stream version)
    [Name, Server] = string:tokens(config:get(jid, Config), "@"),
    Jid = exmpp_jid:make(Name,
                         Server,
                         config:get(resource, Config)),
    exmpp_session:auth_info(Session, Jid, config:get(password, Config)),
    Response = exmpp_session:connect_TCP(Session, Server, config:get(port, Config)),
    case Response of
        {ok, _StreamID, _Features} -> ok;
        _Else -> lager:debug("~p", [Response])
    end,
    exmpp_session:login(Session, config:get(sasl_auth, Config)),
    exmpp_session:send_packet(Session,
                              exmpp_presence:set_status(
                                exmpp_presence:available(),
                                config:get(status, Config))
                             ),
    gen_server:cast(core, {connected, self(), State#state.name}),
    {ok, State#state{session = Session,
                     config_ets = ets:new(config, [bag])}}.

handle_call(Any, _From, State) ->
    lager:info("Recieved unknown call: ~p", [Any]),
    {noreply, State}.

handle_cast(start_plugins, State) ->
    Config = State#state.config_ets,
    PluginList = config:get(plugins, Config),
    case PluginList of
        [] ->
            lager:info("No plugins found"),
            PluginSupervisorPid = undefined;
        NonEmptyList when is_list(NonEmptyList) ->
            lager:debug("statring plugin supervisor"),
            {ok, PluginSupervisorPid} =
                supervisor:start_child(State#state.supervisor,
                                       {list_to_atom(config:get(jid, Config) ++ "_plugin_supervisor"),
                                        {plugin_supervisor, start_link, []},
                                        transient,
                                        infinity,
                                        supervisor,
                                        [plugin_supervisor]}),
            %% Hook point #1
            lager:debug("Plugin list:"),
            lists:foreach(fun(E) ->
                                  lager:debug("~p", [E]),
                                  E:start(PluginSupervisorPid, Config, self())
                          end,
                          PluginList);
        _ -> lager:error("PluginList has strange value: ~p", [PluginList]),
             PluginSupervisorPid = undefined
    end,
    gen_server:cast(core, {started_plugins, self(), State#state.name}),
    {noreply, State#state{plugin_supervisor=PluginSupervisorPid}};
handle_cast(join_rooms, State) ->
    Config = State#state.config_ets,
    Session = State#state.session,
    RoomList = config:get(rooms, Config),
    lists:foreach(fun(RoomConfig) ->
                          muc_tools:join_groupchat(Session, RoomConfig)
                  end,
                  RoomList),
    gen_server:cast(self(), send_muc_keepalive),
    {noreply, State};
handle_cast({send_message, Message, Recepient}, State) ->
    Config = State#state.config_ets,
    Sender = misc:format_str("~s", [config:get(jid, Config)]),
    MessagePacket = create_packet(Recepient, "", Sender, Message),
    gen_server:cast(self(), {send_packet, MessagePacket}),
    {noreply, State};
handle_cast({send_packet, Packet}, State) ->
    Session = State#state.session,
    exmpp_session:send_packet(Session, Packet),
    {noreply, State};
handle_cast(send_muc_keepalive, State) ->
    %% TODD: whitespace ping goes here
    {noreply, State};
handle_cast({get_ets_config, Key}, State) ->
    Ets = State#state.config_ets,
    Config = ets:lookup(Ets, Key),
    {reply, Config, State};
handle_cast({store_ets_config, Term}, State) ->
    Ets = State#state.config_ets,
    ets:insert(Ets, Term),
    {noreply, State};   
handle_cast(Any, State) ->
    lager:info("Recieved unknown cast: '~p'", [Any]),
    {noreply, State}.

%% XMPP packets are handled via handle_info for some reason
handle_info(#received_packet{packet_type = message, raw_packet = Packet}, State) ->
    Message = message:create(Packet),
    Config = State#state.config_ets,
    %% Here starts actual messages' long journey through all plugins
    process_message(Message, Config),
    {noreply, State};
handle_info(#received_packet{packet_type = _PacketType, raw_packet = _Packet}, State) ->
    %% lager:debug("Resieved XMPP packet~ntype: ~p~npacket: ~p", [PacketType, Packet]),
    {noreply, State};
handle_info(Msg, State) -> 
    lager:info("Recieved unknown message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    Session = State#state.session,
    exmpp_session:stop(Session),
    lager:info("worker ~p with pid ~p terminated.~nReason: ~p",
              [State#state.name, self(), Reason]),
    gen_server:cast(core, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

store_config({Key, Value}) when is_atom(Key) ->
    gen_server:cast(core, {store_config, {Key, Value}}).

store_config(WorkerPid, {Key, Value}) ->
    gen_server:cast(WorkerPid, {store_config, {Key, Value}}).

store_config(WorkerPid, Room, {Key, Value}) when is_pid(WorkerPid)
                                                    andalso is_list(Room) ->
    lager:debug("inserting {~w, ~w} to ~w config", [Key, Value, Room]),
    Rooms = gen_server:cast(WorkerPid, {get_config, rooms}),
    RoomConfig = lists:keyfind(Room, 1, Rooms),
    NewRoomConfig =
        [{Key, Value} |
         [{K, V} || {K, V} <- RoomConfig, K =/= Key]],
    NewRooms =
        [{Room, NewRoomConfig} |
         [{R, C} || {R, C} <- Rooms, R =/= Room]],
    lager:debug("new room config: ~w", [NewRooms]),
    gen_server:cast(WorkerPid, {store_config, {rooms, NewRooms}}).

get_config(Key) when is_atom(Key) ->
    gen_server:cast(core, {get_config, Key}).

get_config(WorkerPid, Key) ->
    gen_server:cast(WorkerPid, {get_config, Key}).

get_config(WorkerPid, Room, Key) ->
    Rooms = gen_server:cast(WorkerPid, {get_config, rooms}),
    RoomConfig = lists:keyfind(Room, 1, Rooms),
    config:get(Key, RoomConfig).

process_message(Message, Config) ->
    Plugins = config:get(plugins, Config),
    lists:foreach(fun(Plugin) ->
                          %% lager:debug("passing message in plugin ~s", [Plugin]),
                          %% Hook point #2
                          Plugin:process_message(Message, Config)
                  end,
                  Plugins).
reply(Message, Recepient) ->
    %% lager:debug("replying ~p with ~p", [Recepient, Message]),
    gen_server:cast(self(), {send_message, Message, Recepient}).

%% Low-level xmpp package creation

create_packet(RecepientJid, RecepientResource, Sender, Reply) ->
    case RecepientResource of
        "" ->
            Recepient = list_to_binary(RecepientJid),
            Packet1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, Reply);
        _AnyOther ->
            Recepient = list_to_binary(RecepientJid ++ "/" ++ RecepientResource),
            Packet1 = exmpp_message:make_chat(?NS_JABBER_CLIENT, Reply)
    end,
    Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
    Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Recepient),
    Packet3.
