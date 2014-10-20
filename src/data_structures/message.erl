-module(message).
-export([create/1, type/1, body/1, from/1, timestamp/1, raw/1, from_room/1]).

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

timestamp(Message) ->
    maps:get(timestamp, Message).
    
