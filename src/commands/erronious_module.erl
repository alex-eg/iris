-module(erronious_module).
-export([run/1]).

run(_ArgumentString) ->
    ulog:info("Commiting suicide!"),
    1 / 0,
    ok.
