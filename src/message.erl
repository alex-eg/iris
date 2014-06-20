-module(message).
-export([create/1, get_type/1, get_body/1, get_from/1, get_timestamp/1]).

-record(message,
        {type,
         body,
         from,
         timestamp
         }).

create(RawMessage) ->
    Type = exmpp_message:get_type(RawMessage),
    From = format_str("~s", [exmpp_xml:get_attribute(RawMessage, <<"from">>, undefined)]),
    Body = format_str("~s", [exmpp_message:get_body(RawMessage)]),
    TimeStamp = calendar:local_time(),
    #message{type = Type,
             from = From,
             body = Body,
             timestamp = TimeStamp}.

get_type(Message) ->
    Message#message.type.

get_body(Message) ->
    Message#message.body.
      
get_from(Message) ->
    Message#message.from.

get_timestamp(Message) ->
    Message#message.timestamp.
    
