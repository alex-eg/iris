-module(greet).
-export([run/2]).
-behavior(iris_command).

run(["@greet"], From) ->
    [_Room|NickList] = string:tokens(From, "/"),
    Nick = string:join(NickList, "/"),
    "Hello, " ++ Nick;
run(["@greet"|ArgList], _) ->
    "Hello, " ++ string:join(ArgList, " ");
run(_, _) -> nope.
