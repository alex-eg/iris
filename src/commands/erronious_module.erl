-module(erronious_module).
-export([run/1]).

run(_ArgumentString) ->
    lager:info("Commiting suicide!"),
    1 / 0,
    ok.
