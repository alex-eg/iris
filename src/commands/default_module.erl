-module(default_module).
-export([run/2]).
-behaviour(iris_command).
-alias("@command_name").

run(_ArgumentString, _SenderJidWithResource) ->
    ok.
