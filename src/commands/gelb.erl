-module(gelb).
-export([run/2]).
-alias("@gelb").
-behaviour(iris_command).

run(_, _) ->
    Response = misc:httpc_request(
                 get,
                 {"http://gelbooru.com/index.php?page=post&s=random", []},
                 [{autoredirect, false}],
                 []),
    process_response(Response).

process_response({Status, Headers, Body}) ->
    {_, URL} = lists:keyfind("location", 1, Headers),
    "http://gelbooru.com/" ++ URL;
process_response({error, _Reason}) ->
    "Sorry, could not retrieve information";
process_response(Any) ->
    lager:warning("Httpc request returned strange result: ~p", [Any]).
