-module(jid_worker).
-behavior(gen_server).
%% entry point
-export([start_link/3]).
%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% module API
-export([reply/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

-record(state,
        {name,
         supervisor,
         config,
         session
        }).

start_link(Config, Name, Supervisor) ->
    State = #state{name = Name,
                   supervisor = Supervisor,
                   config = Config
                  },
    {ok, _Pid} = gen_server:start_link(?MODULE, State, []).

% gen_server callbacks

init(State) ->
    lager:info("Jid worker ~p started and has pid ~p", [State#state.name, self()]),
    Config = State#state.config,
    Session = exmpp_session:start({1, 0}), %% retardation needed to start SSL authorization
    [Name, Server] = string:tokens(jid_config:jid(Config), "@"),
    Jid = exmpp_jid:make(Name,
                         Server,
                         jid_config:resource(Config)),
    exmpp_session:auth_info(Session, Jid, jid_config:password(Config)),
    Response = exmpp_session:connect_TCP(Session, Server, jid_config:port(Config)),
    case Response of
        {ok, _StreamID, _Features} -> ok;
        _Else -> lager:debug("~p", [Response])
    end,
    exmpp_session:login(Session, jid_config:sasl_auth(Config)),
    exmpp_session:send_packet(Session,
                              exmpp_presence:set_status(
                                exmpp_presence:available(),
                                jid_config:status(Config))
                             ),
    gen_server:cast(core, {connected, self(), State#state.name}),
    {ok, State#state{session = Session}}.

handle_call({store_config, Item}, _From, State) ->
    Config = State#state.config,
    NewOtherConfig = [Item|jid_config:other_config(Config)],
    NewConfig = jid_config:update(Config, other_config, NewOtherConfig),
    {reply, ok, State#state{config = NewConfig}};
handle_call(Any, _From, State) ->
    lager:info("Recieved unknown call: ~p", [Any]),
    {noreply, State}.

handle_cast(start_plugins, State) ->
    Config = State#state.config,
    PluginList = jid_config:plugins(Config),
    case PluginList of
        [] ->
            lager:info("No plugins found");
        NonEmptyList when is_list(NonEmptyList) ->
            lager:info("found plugins: ~p", [PluginList]),
            %% Hook point #1
            lager:debug("Plugin list:"),
            lists:foreach(fun(E) ->
                                  lager:debug("~p", [E])
                          end,
                          PluginList),
            lager:debug("statring plugin supervisor"),
            {ok, _Pid} = supervisor:start_child(State#state.supervisor,
                                   {list_to_atom(jid_config:jid(Config) ++ "_plugin_supervisor"),
                                    {plugin_supervisor, start_link, [[{Plugin, Config, self()} || Plugin <- PluginList]]},
                                    transient,
                                    infinity,
                                    supervisor,
                                    [plugin_supervisor]});
        _ -> lager:error("PluginList has strange value: ~p", [PluginList])
    end,
    gen_server:cast(core, {started_plugins, self(), State#state.name}),
    {noreply, State};
handle_cast(join_rooms, State) ->
    Config = State#state.config,
    Session = State#state.session,
    RoomList = jid_config:room_confs(Config),
    lists:foreach(fun(RoomConfig) ->
                          muc_tools:join_groupchat(Session, RoomConfig)
                  end,
                  RoomList),
    gen_server:cast(self(), send_muc_keepalive),
    {noreply, State};
handle_cast({send_message, Message, Recepient}, State) ->
    Config = State#state.config,
    Sender = misc:format_str("~s", [jid_config:jid(Config)]),
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
handle_cast(Any, State) ->
    lager:info("Recieved unknown cast: '~p'", [Any]),
    {noreply, State}.

%% XMPP packets are handled via handle_info for some reason
handle_info(#received_packet{packet_type = message, raw_packet = Packet}, State) ->
    Message = message:create(Packet),
    Config = State#state.config,
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

process_message(Message, Config) ->
    Plugins = jid_config:plugins(Config),
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

