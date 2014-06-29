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
    gen_server:start_link({local, Name}, ?MODULE, State, []).

% gen_server callbacks

init(State) ->
    Config = State#state.config,
    Session = exmpp_session:start({1, 0}), %% retardation needed to start SSL authorization
    [Name, Server] = string:tokens(jid_config:jid(Config), "@"),
    Jid = exmpp_jid:make(Name,
                         Server,
                         jid_config:resource(Config)),
    exmpp_session:auth_info(Session, Jid, jid_config:password(Config)),
    {ok, _StreamID, _Features} = exmpp_session:connect_TCP(Session,
                                                           Server,
                                                           jid_config:port(Config)),
    exmpp_session:login(Session, "DIGEST-MD5"),
    exmpp_session:send_packet(Session,
                              exmpp_presence:set_status(
                                exmpp_presence:available(),
                                jid_config:status(Config))
                             ),
    NewState = State#state{
                 config = Config,
                 session = Session
                },
    gen_server:cast(core, {connected, self(), State#state.name}),
    {ok, NewState}.

handle_call(Any, _From, State) ->
    ulog:info("Recieved unknown call: ~p", [Any]),
    {noreply, State}.

handle_cast(start_plugins, State) ->
    Config = State#state.config,
    PluginList = jid_config:plugins(Config),
    ulog:info(State#state.name, "found plugins: ~p", [PluginList]),
    lists:foreach(fun(Plugin) ->
                          ulog:info(State#state.name, "starting plugin: ~p", [Plugin]),
                          %% Hook point #1
                          Plugin:start(State#state.supervisor,
                                       Config,
                                       self())
                  end,
                  PluginList),
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
    ulog:info("Recieved unknown cast: '~p'", [Any]),
    {noreply, State}.

%% XMPP packets are handled via handle_info for some reason
handle_info(#received_packet{packet_type = message, raw_packet = Packet}, State) ->
    Message = message:create(Packet),
    Config = State#state.config,
    %% Here starts actual messages' long journey through all plugins
    process_message(Message, Config),
    {noreply, State};
handle_info(#received_packet{packet_type = PacketType, raw_packet = Packet}, State) ->
    %% ulog:debug("Resieved XMPP packet~ntype: ~p~npacket: ~p", [PacketType, Packet]),
    {noreply, State};
handle_info(Msg, State) -> 
    ulog:info("Recieved unknown message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    Session = State#state.session,
    exmpp_session:stop(Session),
    ulog:info("worker ~p with pid ~p terminated. Reason: ~p",
              [State#state.name, self(), Reason]),
    gen_server:cast(core, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(Message, Config) ->
    Plugins = jid_config:plugins(Config),
    lists:foreach(fun(Plugin) ->
                          ulog:debug("passing message in plugin ~s", [Plugin]),
                          %% Hook point #2
                          Plugin:process_message(Message, Config)
                  end,
                  Plugins).
reply(Message, Recepient) ->
    gen_server:cast(self(), {send_message, Message, Recepient}).
%% Low-level xmpp package creation

create_packet(RecepientJid, RecepientResource, Sender, Reply) ->
    case RecepientResource of
        "" -> Recepient = list_to_binary(RecepientJid);
        _AnyOther -> Recepient = list_to_binary(RecepientJid ++ "/" ++ RecepientResource)
    end,
    Packet1 = exmpp_message:make_chat(?NS_JABBER_CLIENT, Reply),
    Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
    Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Recepient),
    Packet3.

