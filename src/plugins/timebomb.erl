-module(timebomb).
-behavior(iris_plugin).
-export([start/3, process_message/2]).

start(_Supervisor, _WorkerConfig, _From) ->
    ignore.

process_message(_Msg, _Cfg) ->
    exit("Killed by timebomb").

