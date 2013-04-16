-module(iris_module).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{run, 1}];
behaviour_info(_) ->
    undefined.

