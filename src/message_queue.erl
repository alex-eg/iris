-module(message_queue).
-export([new/0, push/2, push_back/2, pop/1, pop_back/1]).
-export([print/1]).

new() ->
    queue:new().

push(Element, Queue) when is_list(Element) ->
    if queue:length(Queue) <= 20 ->
	    queue:in(Element, Queue);
       true ->
	    Q = queue:out_r(Queue),
	    

push_back(Element, Queue) when is_list(Element) ->
    queue:in_r(Element, Queue).

pop(Queue) ->
    queue:out(Queue).

pop_back(Queue) ->
    queue:out_r(Queue).

print(Queue) ->
    queue:filter(fun(S) -> io:format("~p, ", [S]), true end, Queue),
    ok.
			 
