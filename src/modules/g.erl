-module(g).
-export([respond_to_message/2]).

respond_to_message(Text, LastMessages) ->
    case re:run(Text, "^@g (.*)$", [unicode]) of
	{match, CaptureData} -> 
	    [_,{Start, Length}] = CaptureData,
	    Query = lists:sublist(Text, Start + 1, Length),
	    QueryURL = lists:flatten(io_lib:format("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=~p", [Query])),
	    {ok, {_,_,Result}} = httpc:request(QueryURL),
	    Result;
	_ -> "wut"
    end.
