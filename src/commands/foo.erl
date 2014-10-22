-module(foo).
-export([run/2]).
-behavior(iris_command).
-alias("@foo").

run([], _) ->
    "bar".
