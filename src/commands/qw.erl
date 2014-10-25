-module(qw).
-export([run/2]).
-alias("@qw").
-behavior(iris_command).

run([], From) ->
    qw_last_message(1, From);
run(["1"], From) ->
    qw_last_message(1, From);
run(["2"], From) ->
    qw_last_message(2, From);
run(["3"], From) ->
    qw_last_message(3, From);
run(String, _) ->
    qw(String).

qw_last_message(Num, From) ->
    LastMessage = jid_worker:get_message(From, Num),
    qw(LastMessage).

qw("") ->
    "";
qw(String) ->
    UnicodeString = unicode:characters_to_list(list_to_binary(String)),
    En = "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>?",
    Ru = unicode:characters_to_list(<<"йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,">>),
    [H|_] = UnicodeString,
    InEnglish = lists:member(H, En), %% if true, performing English to Russian conversion
    %% Suddenly! Lisp
    binary_to_list(
      unicode:characters_to_binary(
        qw(UnicodeString, InEnglish, En, Ru, ""))).

qw("", _, _, _, Acc) ->
    lists:reverse(Acc);
qw([First|Second], true, En, Ru, Acc) ->
    Index = index_of(First, En),
    case Index of 
        Num when is_integer(Num) ->
            qw(Second, true, En, Ru, [lists:nth(Num, Ru)|Acc]);
        not_found ->
            qw(Second, true, En, Ru, [First|Acc])
    end;
qw([First|Second], false, En, Ru, Acc) ->
    Index = index_of(First, Ru),
    case Index of 
        Num when is_integer(Num) ->
            qw(Second, false, En, Ru, [lists:nth(Num, En)|Acc]);
        not_found ->
            qw(Second, false, En, Ru, [First|Acc])
    end.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index + 1).
