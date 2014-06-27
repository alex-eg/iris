-module(iris_plugin).

-callback process_message(Message :: term(), Config :: term()) ->
    term().

-callback start(Parent :: term(), Config :: term(), From :: term()) ->
    term().
