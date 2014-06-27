-module(misc).
-export([httpc_request/4]).

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
