-module(message).
-export([create/1, get_type/1, get_body/1, get_from/1, get_timestamp/1]).

create(RawMessage) ->
    Type = exmpp_message:get_type(RawMessage),
    From = format_str("~s", [exmpp_xml:get_attribute(RawMessage, <<"from">>, undefined)]),
    Body = format_str("~s", [exmpp_message:get_body(RawMessage)]),
    TimeStamp = calendar:local_time(),
    #{type => Type,
      from => From,
      body => Body,
      timestamp => TimeStamp}.

get_type(Message) ->
    maps:get(type, Message).

get_body(Message) ->
    maps:get(body, Message).
      
get_from(Message) ->
    maps:get(from, Message).

get_timestamp(Message) ->
    maps:get(timestamp, Message).
    
