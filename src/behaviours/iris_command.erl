-module(iris_command).

-type run_return() :: ok
                    | string().

-callback run(Arguments :: list(string())
                         | [],
              From :: string()) ->
    run_return().
