-module(default_module).
-export([run/2]).
-behaviour(iris_module).

run(_ArgumentString, _SenderJidWithResource) ->
    ok.
