-module(last).
-export([respond_to_message/2]).

respond_to_message(_Argument, LastMessages) ->
    print_messages(LastMessages, "").

print_messages([H|T], Acc) ->
    String = lists:flatten(io_lib:format("~s~s~n", [Acc, H])),
    print_messages(T, String);
print_messages([], Acc) ->
    Acc.

