-module(misc).
-export([httpc_request/4, format_str/2]).

httpc_request(Method, Request, HTTPOptions, Options) ->
    try httpc:request(Method, Request, HTTPOptions, Options) of
        {ok, Response} ->
            Response;
        Any ->
            lager:error("Request failed: ~p", [Any]),
            Any
    catch
        error:Exception ->
            lager:error("Exception ~p occcured!", [Exception])
    end.

format_str(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).
