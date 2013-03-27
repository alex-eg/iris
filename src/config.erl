-module(config).
-export([init/1, get/1]).

get(Id) ->
    [{_, Value}] = ets:lookup(bot_config, Id),
    Value.

init(Filename) ->
    ets:new(bot_config, [named_table]),
    {ok, [{config, Config}]} = file:consult(Filename),
    store(Config, bot_config),
    ulog:info("Read configuration file ~s", [Filename]).

init_jid_config(Filename) ->
    

store([], TableName) ->
    ok;
store([H|T], TableName) ->
    {Id, Value} = H,
    ets:insert(TableName, {Id, Value}),
    store(T, TableName).
    
