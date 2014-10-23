-module(message).
-export([create/1,
         type/1, body/1, from/1, timestamp/1, raw/1, from_room/1, nick/1,
         out/1]).

create(RawMessage) ->
    Type = exmpp_message:get_type(RawMessage),
    [From] = io_lib:format("~s", [exmpp_xml:get_attribute(RawMessage, <<"from">>, undefined)]),
    [Body] = io_lib:format("~s", [exmpp_message:get_body(RawMessage)]),
    TimeStamp = calendar:local_time(),
    #{type => Type,
      from => From,
      body => Body,
      timestamp => TimeStamp,
      raw => RawMessage}.

raw(Message) ->
    maps:get(raw, Message).

type(Message) ->
    maps:get(type, Message).

body(Message) ->
    maps:get(body, Message).
      
from(Message) ->
    maps:get(from, Message).

from_room(Message) ->
    From = from(Message),
    [RoomJid|_] = string:tokens(From, "/"),
    RoomJid.

nick(Message) ->
    case type(Message) of
        chat ->
            undefined;
        groupchat ->
            From = message:from(Message),
            [RoomJid|NickResource] = string:tokens(misc:format_str("~s",[From]),"/"),
            Nick = string:join(NickResource, "/") % In case nick/resource contains '/' characters
    end.

timestamp(Message) ->
    maps:get(timestamp, Message).
    
out(Message) ->
    lager:debug("~nRaw:~p~nType:~p~nBody:~p~nFrom:~p~nFrom room:~p~nTimestamp:~p~n",
                [raw(Message),
                 type(Message),
                 body(Message),
                 from(Message),
                 from_room(Message),
                 timestamp(Message)]).
