-module(ulog).
-export([debug/2, debug/1]).
-export([error/2, error/1]).
-export([info/2, info/1]).
-export([warning/2, warning/1]).

out(Level, Format, Args) ->
    Record = io_lib:format(Format, Args),
    io:format("[~w]<~w> ~s~n", [Level, self(), Record]).

debug(Format, Args) ->
    out(dbg, Format, Args).

debug(Args) ->
    io:format("[~w]<~w> ~ts~n",[dbg, self(), Args]).

error(Format, Args) ->
    out(err, Format, Args).

error(Args) ->
    io:format("[~w]<~w> ~ts~n",[err, self(), Args]).

info(Format, Args) ->
    out(inf, Format, Args).

info(Args) ->
    io:format("[~w]<~w> ~ts~n",[inf, self(), Args]).

warning(Record) ->
    out(wrn, "~p", [Record]).

warning(Format, Args) ->
    out(wrn, Format, Args).
