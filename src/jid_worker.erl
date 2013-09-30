-module(jid_worker).
-behavior(gen_server).
%% exntry point
-export([start_link/2]).
%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% module API
-export([get_message/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

-record(state,
        {name,
         config,
         session,
         message_queues % for storing participant messages
        }).

start_link(Config, Name) ->
    State = #state{name = Name,
                   config = Config
                  },
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State) ->
    Config = State#state.config,
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
    NewState = State#state{
                 config = Config,
                 session = Session,
                 message_queues = ets:new(message_queues, [set, named_table])
                },
    log:start_link(?LOG_DIR ++ "/" ++ atom_to_list(State#state.name),
                   atom_to_list(State#state.name) ++ "_logger"),
    gen_server:cast(core, {connected, self(), State#state.name}),
    {ok, NewState}.

handle_call(Any, _From, State) ->
    ulog:info("Recieved UNKNOWN request: ~p", [Any]),
    {noreply, State}.

handle_cast(join_rooms, State) ->
    Config = State#state.config,
    Session = State#state.session,
    RoomList = config:get_room_list(Config),
    lists:foreach(fun(RoomTuple) ->
                          muc_tools:join_groupchat(Session, RoomTuple)
                  end,
                  RoomList),
    gen_server:cast(self(), send_muc_keepalive),
    {noreply, State};
handle_cast(send_muc_keepalive, State) ->
    Config = State#state.config,
    Session = State#state.session,
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
    Session = State#state.session,
    exmpp_session:send_packet(Session, Packet),
    {noreply, State};
handle_cast({store_message, Message, From}, State) ->
    Queue = ets:lookup(message_queues, From),
    update_queue(Queue, Message, From),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved UNKNOWN cast: '~p'", [Any]),
    {noreply, State}.

%% XMPP packets are handled via handle_info for some reason
handle_info(_Msg = #received_packet{packet_type = message, raw_packet = Packet}, State) ->
    Type = exmpp_message:get_type(Packet), %% <- returns 'chat' or 'groupchat'
    %% Here starts actual messages' long journey through modules
    Config = State#state.config,
    process_message(Type, Packet, Config),
    From = format_str("~s", [exmpp_xml:get_attribute(Packet, <<"from">>, undefined)]),
    Body = format_str("~s", [exmpp_message:get_body(Packet)]),
    gen_server:cast(self(), {store_message, Body, From}),
    {noreply, State};
handle_info(_Msg = #received_packet{packet_type = iq}, State) ->
    {noreply, State};
handle_info(_Msg = #received_packet{packet_type = presence}, State) ->
    {noreply, State};
handle_info(Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    Session = State#state.session,
    exmpp_session:stop(Session),
    gen_server:cast(core, {terminated, self(), Reason}),
    ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(chat, Packet, Config) ->
    %% FUTURE: add ignore list to forbid unwanted individuals calling modules
    process_chat(Packet, Config);
process_message(groupchat, Packet, Config) ->
    Stamp = exmpp_xml:get_element(Packet, delay),
    process_groupchat(Stamp, Packet, Config).

process_groupchat(undefined, Packet, Config) ->
    Body = exmpp_message:get_body(Packet),
    From = format_str("~s", [exmpp_xml:get_attribute(Packet, <<"from">>, undefined)]),
    Text = format_str("~s", [Body]),
    Match = re:run(Text, "^" ++ ?COMMAND_PREFIX ++ "(\\w*?)($| (.*)$)", [unicode]),
    try process_command(Match, Text, Config, From) of
        nomatch -> ok;
        no_such_command -> ok;
        Reply when is_list(Reply) ->
            NewPacket = create_packet(groupchat, Reply, Packet, Config),
            gen_server:cast(self(), {send_packet, NewPacket})
    catch
        error:Exception ->
            ulog:info("Caught exception while processing command '~s':~n~p~n"
                      "Backtrace: ~p",
                      [Text, Exception, erlang:get_stacktrace()])
    end;
process_groupchat(_Stamp, _Packet, _Config) ->
    ok.

process_chat(Packet, Config) ->
    ulog:debug("Recieved chat packet: ~p", [Packet]),
    Body = exmpp_message:get_body(Packet),
    Text = format_str("~s", [Body]),
    %% simple echo by now
    NewPacket = create_packet(chat, Text, Packet, Config),
    ulog:debug("Sending packet back: ~p", [NewPacket]),
    gen_server:cast(self(), {send_packet, NewPacket}).

process_command(nomatch, _, _, _) ->
    nomatch;
process_command({match, Match}, Text, Config, From) ->
    {ModuleName, ArgString} = extract_info(Match, Text),
    Module = list_to_atom(ModuleName),
    ModuleList = Config#jid_info.modules,
    ModuleExists = lists:member(Module, ModuleList),
    if ModuleExists ->
            Result = Module:run(ArgString, From);
       not ModuleExists ->
            Result = no_such_command
    end,
    Result.

create_packet(Type, Reply, Incoming, Config) ->
    From = exmpp_xml:get_attribute(Incoming, <<"from">>, undefined),
    [Jid|ResourceList] = string:tokens(format_str("~s",[From]),"/"),
    Resource = string:join(ResourceList, "/"), % In case nick/resource contains '/' characters
    Sender = format_str("~s", [Config#jid_info.jid]),
    create_packet(Type, Jid, Resource, Sender, Reply).

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

get_message(Peer, Pos) ->
    QueueList = ets:lookup(message_queues, Peer),
    case QueueList of
        [] -> 
            "No such participant";
        [{Peer, Queue}] ->
            get_nth_message(Queue, Pos)
    end.

update_queue([], Message, From) ->
    Q = queue:new(),
    Q1 = queue:in(Message, Q),
    ets:insert_new(message_queues, {From, Q1});
update_queue([{From, Q}], Message, From) ->
    Q1 = make_new_queue(queue:len(Q), Q, Message),
    ets:delete(message_queues, From),
    ets:insert_new(message_queues, {From, Q1});
update_queue(_, _, _) ->
    ok.


make_new_queue(Len, Q, Message) when Len >= ?QUEUE_SIZE ->
    queue:in(Message, queue:drop(Q));
make_new_queue(_, Q, Message) ->
    queue:in(Message, Q).

get_nth_message(Queue, Pos) ->
    get_nth_message(Queue, Pos, queue:len(Queue)).

get_nth_message(Queue, Pos, QLen) when QLen >= Pos ->
    lists:nth(Pos, lists:reverse(queue:to_list(Queue)));
get_nth_message(_, _, _) ->
    "Wrong message position in queue".

%% Local helpers below

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

extract_info([_, {ModuleStart, ModuleLength}, {_ArgStart, _ArgLength}], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    {Module, ""};
extract_info([_, {ModuleStart, ModuleLength}, {ArgStart, ArgLength}, _], Text) ->
    Module = lists:sublist(Text, ModuleStart + 1, ModuleLength),
    Argument = string:strip(lists:sublist(Text, ArgStart + 1, ArgLength)),
    {Module, Argument}.

