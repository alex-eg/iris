-module(worker).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").
-behavior(gen_server).

init(State) ->
    Config = State,
    Session = exmpp_session:start(),
    Jid = exmpp_jid:make(Config#bot_info.jid,
			 Config#bot_info.server_address,
			 Config#bot_info.status),
    exmpp_session:auth_basic_digest(Session, Jid, Config#bot_info.password),
    {ok, _StreamID} = exmpp_session:connect_TCP(Session,
						Config#bot_info.server_address,
						Config#bot_info.port),
    exmpp_session:login(Session),
    exmpp_session:send_packet(Session,
			      exmpp_presence:set_status(
				exmpp_presence:available(),
				Config#bot_info.status)
			     ),
    gen_server:cast(root, {connected, self()}),
    Last = message_queue:new(),
    {ok, [[Last|Config]|Session]}.

handle_cast({join, Room, Nick}, State) ->
    [_|Session] = State,
    join_groupchat(Session, Room, Nick),
    {noreply, State};
handle_cast({send_packet, Packet}, State) when ?IS_MESSAGE(Packet) ->
    [_|Session] = State,
    exmpp_session:send_packet(Session, Packet),
    {noreply, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(#received_packet{packet_type = message, raw_packet = Packet}, State) when ?IS_MESSAGE(Packet) ->
    [[LastMessages|Config]|Session] = State,
    process_message(Config, Packet, LastMessages),
    PacketBody = exmpp_message:get_body(Packet),
    PacketBodyText = format_str("~s", [PacketBody]),
    NewLastMessages = message_queue:push(PacketBodyText, LastMessages),
    {noreply, [[NewLastMessages|Config]|Session]};
handle_info(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

join_groupchat(XmppSession, Room, Nick) ->
    ulog:info("Joining ~s as ~s", [Room, Nick]),
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE, <<"to">>, list_to_binary(Room ++ "/" ++ Nick)),
    Presence = exmpp_xml:append_child(BasePresence,
                           #xmlel{name = x, attrs = [#xmlattr{name = <<"xmlns">>, value = ?NS_MUC_b}]
                                 }
				     ),
    exmpp_session:send_packet(XmppSession, Presence).

process_message(Config, Packet, LastMessages) ->
    Type =  exmpp_message:get_type(Packet),
    respond_to_message(Type, Packet, Config, LastMessages).

respond_to_message(groupchat, Packet, Config, LastMessages) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, undefined),
    Body = exmpp_message:get_body(Packet),
    Text = format_str("~s", [Body]),
    case re:run(Text, "^@(\\w.*?) (.*)$", [unicode]) of
	{match, Capture} ->
	    {ModuleName, Argument} = extract_info(Capture, Text),
	    Modules = Config#bot_info.modules,
	    Module = list_to_atom(ModuleName),
	    IsMember = lists:member(Module, Modules),
	    if IsMember -> 
		    Response = Module:respond_to_message(Argument, LastMessages),
		    [Room, Nick] = string:tokens(format_str("~s",[From]),"/"),
		    ResponseBody = Nick ++ ", " ++ Response,
		    NewTo = list_to_binary(Room),
		    NewFrom = format_str("~s", [Config#bot_info.jid ++ 
						    "@" ++ 
						    Config#bot_info.server_address]),
		    P1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, ResponseBody),
		    P2 = exmpp_xml:set_attribute(P1, <<"from">>, NewFrom),
		    P3 = exmpp_xml:set_attribute(P2, <<"to">>, NewTo),
		    gen_server:cast(worker, {send_packet, P3});
	       true -> ok
	    end;
	_ -> ok
    end;
respond_to_message(_, _, _, _) ->
    ok.

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

extract_info([_, {ModuleStart, ModuleLength}, {ArgStart, ArgLength}], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    Argument = lists:sublist(Text, ArgStart + 1, ArgLength),
    {Module, Argument}.
