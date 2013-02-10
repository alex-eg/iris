-module(g).
-export([respond_to_message/2]).

respond_to_message(Argument, _LastMessages) ->
    QueryURL = lists:flatten(io_lib:format("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=~p", [Argument])),
    {ok, {_,_,Result}} = httpc:request(QueryURL),
    Result.
   
