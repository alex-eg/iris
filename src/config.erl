-module(config).
-export([init/1, get/1]).

get(Id) ->
    [{_, Value}] = ets:lookup(bot_config, Id),
    Value.

init(Filename) ->
    ets:new(bot_config, [named_table]),
    {ok, [{config, Config}]} = file:consult(Filename),
    store(Config),
    ulog:info("Read configuration file ~s", [Filename]).

store([]) ->
    ok;
store([H|T]) ->
    {Id, Value} = H,
    ets:insert(bot_config, {Id, Value}),
    store(T).
    
