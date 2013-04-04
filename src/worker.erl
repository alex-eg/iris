-module(worker).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").
-behavior(gen_server).

init(State) ->
    Config = State,
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
    gen_server:cast(root, {connected, self()}),
    {ok, [Config|Session]}.

handle_cast(join_rooms, State) ->
    [Config|Session] = State,
    RoomList = config:get_room_list(Config),
    %% Need a closure here, because foreach accepts only one argument functions
    ulog:debug("Acquired room list:~p~n", [RoomList]),
    JoinLambda = fun(RoomTuple) ->
			 muc_tools:join_groupchat(Session, RoomTuple)
		 end,
    lists:foreach(JoinLambda,
		  RoomList),
    gen_server:cast(self(), send_muc_keepalive),
    {noreply, State};
handle_cast(send_muc_keepalive, State) ->
    [Config|Session] = State,
    RoomList = config:get_room_list(Config),
    Lambda = fun(RoomTuple) ->
		     muc_tools:send_muc_keepalive(Session, RoomTuple)
	     end,
    lists:foreach(Lambda,
		  RoomList),
    timer:apply_after(?REJOIN_TIMEOUT,
		      gen_server,
		      cast,
		      [self(), send_muc_keepalive]
		     ),
    {noreply, State};
handle_cast({send_packet, Packet}, State) when ?IS_MESSAGE(Packet) ->
    [_|Session] = State,
    exmpp_session:send_packet(Session, Packet),
    {noreply, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(_Msg = #received_packet{packet_type = message, raw_packet = Packet}, State) ->
    ulog:debug("Recieved message of type MESSAGE: ~p~n", [_Msg]),
    [Config|Session] = State,
    process_message(Config, Packet),
    PacketBody = exmpp_message:get_body(Packet),
    PacketBodyText = format_str("~s", [PacketBody]),
    {noreply, [Config|Session]};
handle_info(_Msg, State) -> 
    ulog:debug("Recieved message: ~p~n", [_Msg]),
    {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

process_message(Config, Packet) ->
    Type =  exmpp_message:get_type(Packet),
    respond_to_message(Type, Packet, Config).

respond_to_message(groupchat, Packet, Config) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, undefined),
    Body = exmpp_message:get_body(Packet),
    Text = format_str("~s", [Body]),
    case re:run(Text, "^@(\\w.*?) (.*)$", [unicode]) of
	{match, Capture} ->
	    {ModuleName, Argument} = extract_info(Capture, Text),
	    Modules = Config#jid_info.modules,
	    Module = list_to_atom(ModuleName),
	    IsMember = lists:member(Module, Modules),
	    if IsMember -> 
		    Response = Module:respond_to_message(Argument),
		    [Room, Nick] = string:tokens(format_str("~s",[From]),"/"),
		    ResponseBody = Nick ++ ", " ++ Response,
		    NewTo = list_to_binary(Room),
		    NewFrom = format_str("~s", [Config#jid_info.jid]),
		    P1 = exmpp_message:make_groupchat(?NS_JABBER_CLIENT, ResponseBody),
		    P2 = exmpp_xml:set_attribute(P1, <<"from">>, NewFrom),
		    P3 = exmpp_xml:set_attribute(P2, <<"to">>, NewTo),
		    gen_server:cast(worker, {send_packet, P3});
	       true -> ok
	    end;
	_ -> ok
    end;
respond_to_message(_, _, _)->
    ok.

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

extract_info([_, {ModuleStart, ModuleLength}, {ArgStart, ArgLength}], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    Argument = lists:sublist(Text, ArgStart + 1, ArgLength),
    {Module, Argument}.
