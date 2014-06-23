-module(chat_commands).
-behaviour(iris_plugin).

-export([process_message/2]).

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
    Stamp = exmpp_xml:get_element(Message, delay), %% removing history messages
    case Stamp of
        undefined -> process_groupchat(Message, Config);
        _ -> ok
    end.

process_groupchat(Message, Config) ->
    RoomList = jid_config:rooms(Config),
    FromRoom = message:from(Message),
    ulog:info("Message from room ~s", [FromRoom]).
