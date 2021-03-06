-module(config).
-export([get/2]).

-include("xmpp.hrl").

get(Key, Proplist) when is_list(Proplist) ->
    proplists:get_value(Key, Proplist);
get(Key, Ets) ->
    case ets:lookup(Ets, Key) of
        [] ->
            undefined;
        [{Key, Value}|_] ->
            Value
    end.
