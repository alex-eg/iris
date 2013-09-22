-module(gelb).
-export([run/1]).
-behaviour(iris_module).

run(_) ->
    {ok, {_Status,
          Headers,
          _Body}} = httpc:request(get, {"http://gelbooru.com/index.php?page=post&s=random", []}, 
                                  [{autoredirect, false}], 
                                  []),
    {_, URL} = lists:keyfind("location", 1, Headers),
    "http://gelbooru.com/" ++ URL.
