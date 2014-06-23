-module(iris_plugin).

-callback process_message(Message :: term(), Config :: term()) ->
    term().

-callback init(Config :: term()) ->
    term().
