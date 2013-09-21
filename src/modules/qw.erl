-module(qw).
-export([run/1]).
-behavior(iris_module).

run("") ->
    qw(jid_worker:get_message("channel@conference.coderollers.com/ktt9", 1)).

qw("") ->
    "";
qw(String) ->
    En = "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVfBNM<>?",
    Ru = "йцукенгшщзхъфывапролджэячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,",
    [H|_] = String,
    InEnglish = lists:member(H, En), %% if true, performing English to Russian conversion
    qw(String, InEnglish, En, Ru, "").

qw("", _, _, _, Acc) ->
    Acc;
qw([First|Second], true, En, Ru, Acc) ->
    qw(Second, true, En, Ru, [lists:nth(index_of(First, En), Ru)|Acc]);
qw([First|Second], false, En, Ru, Acc) ->
    qw(Second, true, En, Ru, [lists:nth(index_of(First, Ru), En)|Acc]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).  
        
