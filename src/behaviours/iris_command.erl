-module(iris_command).

%% subject to change
-type chat_callback() :: fun(([char()], [char()]) -> [char()]).
-callback run(A :: term(), B :: term()) ->
    term().
%% -callback module_level() ->
%%     'chat_command' | 'worker_module' | 'global_module'.
%% -callback get_hooks() ->
%%     chat_callback() | fun((...) -> term()).
