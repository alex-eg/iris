-module(default_module).
-export([run/1]).
-behaviour(iris_module).

run(_ArgumentString) ->
    ok.
