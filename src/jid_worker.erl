-module(jid_worker).
-behavior(gen_server).
-export([start_link/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

-record(worker_state,
	{name,
	 config,
	 session
	}).

start_link(Config, Name) ->
    State = #worker_state{name = Name,
			  config = Config},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State) ->
    Config = State#worker_state.config,
    Session = exmpp_session:start(),
    [Name, Server] = string:tokens(Config#jid_info.jid, "@"),
    Jid = exmpp_jid:make(Name,
			 Server,
			 Config#jid_info.resource),
    exmpp_session:auth_basic_digest(Session, Jid, Config#jid_info.password),
    {ok, _StreamID} = exmpp_session:connect_TCP(Session,
						Server,
						Config#jid_info.port),
    exmpp_session:login(Session),
    exmpp_session:send_packet(Session,
			      exmpp_presence:set_status(
				exmpp_presence:available(),
				Config#jid_info.status)
			     ),
    gen_server:cast(root, {connected, self(), State#worker_state.name}),
    NewState = #worker_state{
		  name = State#worker_state.name,
		  config = Config,
		  session = Session},
    {ok, NewState}.
    
handle_cast(join_rooms, State) ->
    Config = State#worker_state.config,
    Session = State#worker_state.session,
    RoomList = config:get_room_list(Config),
    lists:foreach(fun(RoomTuple) ->
			  muc_tools:join_groupchat(Session, RoomTuple)
		  end,
		  RoomList),
    gen_server:cast(self(), send_muc_keepalive),
    {noreply, State};
handle_cast(send_muc_keepalive, State) ->
    Config = State#worker_state.config,
    Session = State#worker_state.session,
    RoomList = config:get_room_list(Config),
    lists:foreach(fun(RoomTuple) ->
			  muc_tools:send_muc_keepalive(Session, RoomTuple)
		  end,
		  RoomList),
    timer:apply_after(?REJOIN_TIMEOUT,
		      gen_server,
		      cast,
		      [self(), send_muc_keepalive]
		     ),
    {noreply, State};
handle_cast({send_packet, Packet}, State) ->
    Session = State#worker_state.session,
    exmpp_session:send_packet(Session, Packet),
    {noreply, State};
handle_cast(_, State) -> {noreply, State}.

%% XMPP packages are handled via handle_info for some reason
handle_info(_Msg = #received_packet{packet_type = message, raw_packet = Packet}, State) ->
    Type = exmpp_message:get_type(Packet), %% <- returns 'chat' or 'groupchat'
    %% Here starts actual messages' long journey through modules
    Config = State#worker_state.config,
    process_message(Type, Packet, Config),
    {noreply, State};
handle_info(_Msg = #received_packet{packet_type = iq}, State) ->
    {noreply, State};
handle_info(_Msg = #received_packet{packet_type = presence}, State) ->
    {noreply, State};
handle_info(Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: '~p'", [Msg]),
    {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.

terminate(Reason, State) ->
    Session = State#worker_state.session,
    exmpp_session:stop(Session),
    gen_server:cast(root, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(chat, Packet, Config) ->
    %% FUTURE: add ignore list to forbid unwanted individuals calling modules
    process_message(groupchat, Packet, Config);
process_message(groupchat, Packet, Config) ->
    Stamp = exmpp_xml:get_element(Packet, delay),
    process_groupchat(Stamp, Packet, Config).

process_groupchat(undefined, Packet, Config) ->
    Body = exmpp_message:get_body(Packet),
    Text = format_str("~s", [Body]),
    Match = re:run(Text, "^" ++ ?COMMAND_PREFIX ++ "(\\w*?)($| (.*)$)", [unicode]),
    case process_command(Match, Text, Config) of
	nomatch -> ok;
	no_such_command -> ok;
	Reply when is_list(Reply) ->
	    NewPacket = create_packet(Reply, Packet, Config),
	    gen_server:cast(self(), {send_packet, NewPacket})
    end;
process_groupchat(_Stamp, _Packet, _Config) ->
    ok.

process_command(nomatch, _, _) ->
    nomatch;
process_command({match, Match}, Text, Config) ->
    {ModuleName, ArgString} = extract_info(Match, Text),
    Module = list_to_atom(ModuleName),
    ModuleList = Config#jid_info.modules,
    ModuleExists = lists:member(Module, ModuleList),
    if ModuleExists ->
	    Result = Module:run(ArgString);
       not ModuleExists ->
	    Result = no_such_command
    end,
    Result.
			    
create_packet(Reply, Incoming, Config) ->
    From = exmpp_xml:get_attribute(Incoming, <<"from">>, undefined),
    [Room, Nick] = string:tokens(format_str("~s",[From]),"/"),
    Body = Nick ++ ", " ++ Reply,
    Reciever = list_to_binary(Room),
    Sender = format_str("~s", [Config#jid_info.jid]),
    Packet1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, Body),
    Packet2 = exmpp_xml:set_attribute(Packet1, <<"from">>, Sender),
    Packet3 = exmpp_xml:set_attribute(Packet2, <<"to">>, Reciever),
    Packet3.

%% Local helpers below

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

extract_info([_, {ModuleStart, ModuleLength}, {ArgStart, ArgLength}], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    {Module, ""};
extract_info([_, {ModuleStart, ModuleLength}, {ArgStart, ArgLength}, _], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    Argument = string:strip(
		 lists:sublist(Text, ArgStart + 1, ArgLength)
		),
    {Module, Argument}.

