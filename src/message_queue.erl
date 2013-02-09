-module(message_queue).
-export([new/0, push/2, push_back/2, pop/1, pop_back/1]).
-export([print/1]).

-define(MAXLENGTH, 5).

new() ->
    [].

push(Element, Queue) when is_list(Element) ->
    Length = length(Queue),
    if Length < ?MAXLENGTH ->
	    [Element|Queue];
       Length >= ?MAXLENGTH ->
	    [_|Head] = lists:reverse(Queue),
	    [Element|lists:reverse(Head)]
    end.
	    
push_back(Element, Queue) when is_list(Element) ->
    Length = length(Queue),
    if Length < ?MAXLENGTH ->
	    [Queue|[Element]];
       Length >= ?MAXLENGTH ->
	    [_|Tail] = Queue,
	    [Tail|[Element]]
    end.

pop([]) ->
    {empty, []};
pop(Queue) ->
    [H|T] = Queue,
    {H,T}.

pop_back([]) ->
    {empty, []};
pop_back(Queue) ->
    [H|T] = lists:reverse(Queue),
    {H,lists:reverse(T)}.

print(Queue) ->
    lists:map(fun(S) -> io:format("~p, ", [S]) end, Queue),
    ok.
			 
