-module(message_queue).
-export([new/0, push/2, push_back/2, pop/1, pop_back/1]).
-export([print/1]).

new() ->
    [].

push(Element, Queue) when is_list(Element) ->
    Length = queue:len(Queue),
    if Length < 20 ->
	    queue:in(Element, Queue);
       Length >= 20 ->
	    {_, Q} = queue:out_r(Queue),
	    queue:in(Element, Q)
    end.
	    
push_back(Element, Queue) when is_list(Element) ->
    Length = queue:len(Queue),
    if Length < 20 ->
	    queue:in_r(Element, Queue);
       Length >= 20 ->
	    {_, Q} = queue:out(Queue),
	    queue:in_r(Element, Q)
    end.

pop(Queue) ->
    queue:out(Queue).

pop_back(Queue) ->
    queue:out_r(Queue).

print(Queue) ->
    queue:filter(fun(S) -> io:format("~p, ", [S]), true end, Queue),
    ok.
			 
