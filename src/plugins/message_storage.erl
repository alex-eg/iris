-module(message_storage).
-behaviour(iris_plugin).

-export([start/3, process_message/2]).

start(_Parent, _Config, _From) ->
    ok.

process_message(Message, Config) ->
       Type = message:type(Message),
    case Type of
        chat ->
            process_chat(Message, Config);
        groupchat ->
            preprocess_groupchat(Message, Config);
        error ->
            lager:warning("got XMPP error stanza: ~p", [message:raw(Message)]);
        _Other ->
            lager:error("got unknown message type: ~s", [Type])
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
    ok.
