-module(gelb).
-export([run/2]).
-alias("@gelb").
-behaviour(iris_command).

run(_, _) ->
    {_Status,
     Headers,
     _Body} = misc:httpc_request(get, {"http://gelbooru.com/index.php?page=post&s=random", []},
                                 [{autoredirect, false}], 
                                 []),
    {_, URL} = lists:keyfind("location", 1, Headers),
    "http://gelbooru.com/" ++ URL.
