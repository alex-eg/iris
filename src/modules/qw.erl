-module(qw).
-export([run/1]).
-behavior(iris_module).

run("")->
    "ok".
