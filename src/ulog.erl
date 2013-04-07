-module(ulog).
-export([debug/2, debug/1]).
-export([info/1, info/2]).
-export([warning/1, warning/2]).

out(Level, Format, Args) ->
    Record = io_lib:format(Format, Args),
    io:format("[~w] ~s~n", [Level, Record]).

debug(Format, Args) ->
    out(dbg, Format, Args).

debug(Args) ->
    io:format("[~w] ~ts~n",[dbg,Args]).

info(Format, Args) ->
    out(inf, Format, Args).

info(Args) ->
    io:format("[~w] ~ts~n",[inf,Args]).

warning(Record) ->
    out(wrn, "~p", [Record]).

warning(Format, Args) ->
    out(wrn, Format, Args).
