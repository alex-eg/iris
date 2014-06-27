-module(misc).
-export([httpc_request/4, format_str/2]).

httpc_request(Method, Request, HTTPOptions, Options) ->
    try httpc:request(Method, Request, HTTPOptions, Options) of
        {ok, Response} ->
            Response;
        Any ->
            ulog:error("Request failed: ~p", [Any])
    catch
        error:Exception ->
            ulog:error("Exception ~p occcured!", [Exception])
    end.

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).
