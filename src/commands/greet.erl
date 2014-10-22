-module(greet).
-export([run/2]).
-behavior(iris_command).
-alias("@greet").

run(_, From) ->
    [_Room|NickList] = string:tokens(From, "/"),
    Nick = string:join(NickList, "/"),
    "Hello, " ++ Nick.
