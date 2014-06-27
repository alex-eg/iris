-module(jid_worker).
-behavior(gen_server).
%% entry point
-export([start_link/2]).
%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% module API
-export([reply/3]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

-record(state,
        {name,
         config,
         session
        }).

start_link(Config, Name) ->
    State = #state{name = Name,
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
handle_cast(send_muc_keepalive, State) ->
    %% TODD: whitespace ping goes here
    {noreply, State};
handle_cast({send_packet, Packet}, State) ->
    Session = State#state.session,
    exmpp_session:send_packet(Session, Packet),
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
    ulog:info("Resieved XMPP packet~ntype: ~p~npacket: ~p", [PacketType, Packet]),
    {noreply, State};
handle_info(Msg, State) -> 
    ulog:info("Recieved unknown message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    Session = State#state.session,
    exmpp_session:stop(Session),
    gen_server:cast(core, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(Message, Config) ->
    Plugins = jid_config:plugins(Config),
    lists:foreach(fun(Plugin) ->
                          ulog:debug("passing message in plugin ~s", [Plugin]),
                          Plugin:process_message(Message, Config)
                  end,
                  Plugins).

reply(Reply, Incoming, Config) ->
    From = exmpp_xml:get_attribute(message:raw(Incoming), <<"from">>, undefined),
    [Jid|ResourceList] = string:tokens(format_str("~s",[From]),"/"),
    Resource = string:join(ResourceList, "/"), % In case nick/resource contains '/' characters
    Sender = format_str("~s", [jid_config:jid(Config)]),
    NewMessage = create_packet(message:type(Incoming), Jid, Resource, Sender, Reply),
    gen_server:cast(self(), {send_packet, NewMessage}).

create_packet(chat, Jid, Resource, Sender, Reply) ->
    Reciever = list_to_binary(Jid ++ "/" ++ Resource),
    Packet1 = exmpp_message:make_chat(?NS_JABBER_CLIENT, Reply),
    Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
    Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Reciever),
    Packet3;    
create_packet(groupchat, Room, Nick, Sender, Reply) ->
    Body = Nick ++ ", " ++ Reply,
    Reciever = list_to_binary(Room),
    Packet1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, Body),
    Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
    Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Reciever),
    Packet3.

