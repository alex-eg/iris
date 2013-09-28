-module(gelb).
-export([run/2]).
-behaviour(iris_module).

run(_, _) ->
    {_Status,
     Headers,
     _Body} = gen_server:call(core, 
                              {get_http, get, {"http://gelbooru.com/index.php?page=post&s=random", []}, 
                               [{autoredirect, false}], 
                               []}),
    {_, URL} = lists:keyfind("location", 1, Headers),
    "http://gelbooru.com/" ++ URL.
