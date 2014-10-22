-module(exit).
-export([run/2]).
-behavior(iris_command).
-alias("@exit").

run([], _) ->
    exit("I SAID SO!");
run(_, _) ->
    nope.

   
