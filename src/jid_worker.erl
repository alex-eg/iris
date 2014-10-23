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

-define(CONFIG_ETS_NAME, config).

start_link(Config, Name, Supervisor) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, {Config, Name, Supervisor}, []).

% gen_server callbacks

init({Config, WorkerName, Supervisor}) ->
    ConfigEts = ets:new(config, [named_table, set]),
    lager:debug("initial config: ~p", [Config]),
    lists:foreach(fun(Entry) ->
                          lager:debug("Inserting ~p in worker config ets", [Entry]),
                          ets:insert(?CONFIG_ETS_NAME, Entry)
                  end,
                  Config),

    lager:info("Jid worker ~p started and has pid ~p", [WorkerName, self()]),
    Session = exmpp_session:start({1, 0}), %% retardation needed to start SSL authorization (designates xmpp stream version)
    [Login, Server] = string:tokens(config:get(jid, ConfigEts), "@"),
    Jid = exmpp_jid:make(Login,
                         Server,
                         config:get(resource, ConfigEts)),
    exmpp_session:auth_info(Session, Jid, config:get(password, ConfigEts)),
    Response = exmpp_session:connect_TCP(Session, Server, config:get(port, ConfigEts)),
    case Response of
        {ok, _StreamID, _Features} -> ok;
        _Else -> lager:debug("~p", [Response])
    end,
    exmpp_session:login(Session, config:get(sasl_auth, ConfigEts)),
    exmpp_session:send_packet(Session,
                              exmpp_presence:set_status(
                                exmpp_presence:available(),
                                config:get(status, ConfigEts))
                             ),
    gen_server:cast(core, {connected, self(), WorkerName}),
    State = #state{name = WorkerName,
                   supervisor = Supervisor,
                   config_ets = ConfigEts,
                   session = Session
                  },
    {ok, State}.

handle_call({get_config, Key}, _From, State) ->
    Reply = config:get(Key, State#state.config_ets),
    {reply, Reply, State};
handle_call(Any, _From, State) ->
    lager:info("Recieved unknown call: ~p", [Any]),
    {noreply, State}.

handle_cast(start_plugins, State) ->
    Config = State#state.config_ets,
    PluginList = config:get(plugins, Config),
    {_, PluginSupervisorPid, _, _} = lists:keyfind([plugin_supervisor], 4, supervisor:which_children(State#state.supervisor)),
    case PluginList of
        [] ->
            lager:info("No plugins found");
        NonEmptyList when is_list(NonEmptyList) ->
            %% Hook point #1
            lager:info("Plugin list:"),
            lists:foreach(fun(E) -> lager:info("-- ~p", [E]) end, PluginList),
            lists:foreach(fun(E) ->
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
handle_cast({store_config, {Key, Value}}, State) ->
    ets:insert(State#state.config_ets, {Key, Value}),
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
    %% lager:info("worker ~p with pid ~p terminated.~nReason: ~p",
    %%           [State#state.name, self(), Reason]),
    %% gen_server:cast(core, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

store_config({Key, Value}) when is_atom(Key) ->
    gen_server:cast(core, {store_config, {Key, Value}}).

store_config(WorkerPid, {Key, Value}) ->
    gen_server:cast(WorkerPid, {store_config, {Key, Value}}).

store_config(WorkerPid, Room, {Key, Value}) when is_pid(WorkerPid)
                                                    andalso is_list(Room) ->
    if
        WorkerPid == self() ->
            Rooms = config:get(rooms, ?CONFIG_ETS_NAME);
        WorkerPid /= self() ->
            Rooms = gen_server:call(WorkerPid, {get_config, rooms})
    end,
    [RoomConfig] = [Rest || [{jid, RoomJid} | Rest] <- Rooms, RoomJid == Room],
    NewRoomConfig =
        [{Key, Value} |
         [{K, V} || {K, V} <- RoomConfig, K /= Key]],
    NewRooms =
        [[{jid, Room} | NewRoomConfig] |
         [[{jid, RoomJid} | Rest] || [{jid, RoomJid} | Rest] <- Rooms, RoomJid /= Room]],
    if
        WorkerPid == self() ->
            ets:insert(?CONFIG_ETS_NAME, {rooms, NewRooms});
        WorkerPid /= self() ->
            gen_server:cast(WorkerPid, {store_config, {rooms, NewRooms}})
    end.

get_config(Key) when is_atom(Key) ->
    gen_server:call(core, {get_config, Key}).

get_config(WorkerPid, Key) ->
    if
        WorkerPid == self() ->
            config:get(Key, ?CONFIG_ETS_NAME);
        WorkerPid /= self() ->
            gen_server:call(WorkerPid, {get_config, Key})
    end.

get_config(WorkerPid, Room, Key) ->
    if
        WorkerPid == self() ->
            Rooms = config:get(rooms, ?CONFIG_ETS_NAME);
        WorkerPid /= self() ->
            Rooms = gen_server:call(WorkerPid, {get_config, rooms})
    end,
    [RoomConfig] = [[{jid, RoomJid} | Rest] || [{jid, RoomJid} | Rest] <- Rooms, RoomJid == Room],
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
