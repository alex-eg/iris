-module(exit).
-export([run/2]).
-behavior(iris_command).

run(["@exit"], _) ->
    exit("I SAID SO!");
run(_, _) ->
    nope.

   
