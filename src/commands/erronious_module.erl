-module(erronious_module).
-behavior(iris_command).
-export([run/2]).
-alias("@suicide").

run(_, _) ->
    lager:info("Commiting suicide!"),
    exit(badarith),
    ok.
