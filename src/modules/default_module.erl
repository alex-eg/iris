-module(default_module).
-export([run/2]).
-behaviour(iris_command).

run(_ArgumentString, _SenderJidWithResource) ->
    ok.
