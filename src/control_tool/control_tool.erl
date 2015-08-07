-module(control_tool).
-export([main/1]).

main(["stop"]) ->
    net_adm:ping(iris@localhost),
    rpc:call(iris@localhost, application, stop, [iris]),
    rpc:call(iris@localhost, erlang, halt, []),
    erlang:halt();
main(_) ->
    usage().

usage() ->
    io:format("usage: control_tool [option] <command>~n"
              "~n"
              "Commands:~n"
              "~n"
              "start           start bot~n"
              "stop            gracefully stop bot~n").
