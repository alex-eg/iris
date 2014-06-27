-module(ulog).
-export([debug/3, debug/2, debug/1]).
-export([error/3, error/2, error/1]).
-export([info/3, info/2, info/1]).
-export([warning/3, warning/2, warning/1]).

out(Level, Format, Args) ->
    Record = io_lib:format(Format, Args),
    io:format("[~w] ~s~n", [Level, Record]).

debug(From, Format, Args) ->
    NewFormat = misc:format_str("<~s>: ~s", [From, Format]),
    debug(NewFormat, Args).

debug(Format, Args) ->
    out(dbg, Format, Args).

debug(Args) ->
    io:format("[~w] ~ts~n",[dbg, Args]).

error(From, Format, Args) ->
    NewFormat = misc:format_str("<~s>: ~s", [From, Format]),
    out(err, NewFormat, Args).

error(Format, Args) ->
    out(err, Format, Args).

error(Args) ->
    io:format("[~w] ~ts~n",[err, Args]).

info(From, Format, Args) ->
    NewFormat = misc:format_str("<~s>: ~s", [From, Format]),
    info(NewFormat, Args).

info(Format, Args) ->
    out(inf, Format, Args).

info(Args) ->
    io:format("[~w] ~ts~n",[inf, Args]).

warning(From, Format, Args) ->
    NewFormat = misc:format_str("<~s>: ~s", [From, Format]),
    warning(NewFormat, Args).

warning(Record) ->
    out(wrn, "~p", [Record]).

warning(Format, Args) ->
    out(wrn, Format, Args).
