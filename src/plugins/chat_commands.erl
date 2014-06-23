-module(chat_commands).
-behaviour(iris_plugin).

-export([process_message/2]).

-define(DEFAULT_COMMAND_PREFIX, "@").

process_message(Message, Config) ->
    Type = message:type(Message),
    case Type of 
        chat ->
            process_chat(Message, Config);
        groupchat ->
            preprocess_groupchat(Message, Config);
        Other ->
            ulog:error("Got stragne message type: ~s", [Type])
    end.

process_chat(Message, Config) ->            
    ok.

preprocess_groupchat(Message, Config) ->
    Stamp = exmpp_xml:get_element(message:raw(Message), delay), %% removing history messages
    case Stamp of
        undefined -> process_groupchat(Message, Config);
        _ -> ok
    end.

process_groupchat(Message, Config) ->
    _RoomList = jid_config:room_confs(Config),
    FromRoom = message:from(Message),
    ulog:info("Message from room ~s", [FromRoom]).


%% !!moved from jid_worker!!

%% process_message(chat, Message, Config) ->
%%     process_chat(Message, Config);
%% process_message(groupchat, Message, Config) ->
%%     Stamp = exmpp_xml:get_element(Message, delay), %% removing history messages
%%     case Stamp of
%%         undefined -> process_groupchat(Message, Config);
%%         _ -> ok
%%     end.

%% process_groupchat(Message, Config) ->
%%     Body = exmpp_message:get_body(Message),
%%     From = format_str("~s", [exmpp_xml:get_attribute(Message, <<"from">>, undefined)]),
%%     Text = format_str("~s", [Body]),
%%     Match = re:run(Text, "^" ++ ?DEFAULT_COMMAND_PREFIX ++ "(\\w*?)($| (.*)$)", [unicode]),
%%     try process_command(Match, Text, Config, From) of
%%         nomatch -> ok;
%%         no_such_command -> ok;
%%         Reply when is_list(Reply) ->
%%             NewMessage = create_packet(groupchat, Reply, Message, Config),
%%             gen_server:cast(self(), {send_packet, NewMessage})
%%     catch
%%         error:Exception ->
%%             ulog:info("Caught exception while processing command '~s':~n~p~n"
%%                       "Backtrace: ~p",
%%                       [Text, Exception, erlang:get_stacktrace()])
%%     end.

%% process_chat(Message, Config) ->
%%     ulog:debug("Recieved chat packet: ~p", [Message]),
%%     Body = exmpp_message:get_body(Message),
%%     Text = format_str("~s", [Body]),
%%     %% simple echo by now
%%     NewMessage = create_packet(chat, Text, Message, Config),
%%     ulog:debug("Sending packet back: ~p", [NewMessage]),
%%     gen_server:cast(self(), {send_packet, NewMessage}).

%% process_command(nomatch, _, _, _) ->
%%     nomatch;
%% process_command({match, Match}, Text, Config, From) ->
%%     {ModuleName, ArgString} = extract_info(Match, Text),
%%     Module = list_to_atom(ModuleName),
%%     ModuleList = jid_config:modules(Config),
%%     ModuleExists = lists:member(Module, ModuleList),
%%     if ModuleExists ->
%%             Result = Module:run(ArgString, From);
%%        not ModuleExists ->
%%             Result = no_such_command
%%     end,
%%     Result.
