-module(url_title).
-behaviour(iris_plugin).

-export([start/3, process_message/2]).

start(_Parent, _Config, _From) ->
    okay.

process_message(Message, Config) ->
    Type = message:type(Message),
    case Type of 
        groupchat ->
            preprocess_groupchat(Message, Config);
            %ulog:warning(?MODULE, "Message: ~p", [message:raw(Message)]);
        error ->
            ulog:warning(?MODULE, "got XMPP error stanza: ~p", [message:raw(Message)]);
        _Other ->
            ulog:error(?MODULE, "got unknown message type: ~s", [Type])
    end.

preprocess_groupchat(Message, Config) ->
    Stamp = exmpp_xml:get_element(message:raw(Message), delay), %% removing history messages
    case Stamp of
        undefined -> process_groupchat(Message, Config);
        _ -> ok
    end.

process_groupchat(Message, Config) ->
    RoomConfList = jid_config:room_confs(Config),
    FromRoom = message:from_room(Message),
    %% ulog:debug("Message from room ~s", [FromRoom]),
    Response = string:tokens(message:body(Message), " "),
    From = exmpp_xml:get_attribute(message:raw(Message), <<"from">>, undefined),
    [RoomJid|NickResource] = string:tokens(misc:format_str("~s",[From]),"/"),
    Result = re:run(Response, "(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})(\/(?:[\/\w \.-]*)*\/?)?([\/?#].*)?"),
    case Result of
        nomatch    -> ok;
        {match, _} -> jid_worker:reply(lists:flatten(io_lib:format("~p", [Result])) ++ message:body(Message), RoomJid)
    end.

%string:substr("Hello, World!", 8, 12),