-module(config).
-export([read/1, parse/2]).

-include("xmpp.hrl").

read(Filename) ->
    {ok, [ConfigList]} = file:consult(Filename),
    ulog:info("Read configuration file ~s", [Filename]),
    ConfigList.

parse(jid_config, {Jid, Config}) ->
    Port = proplists:get_value(port, Config, 5222),
    Resource = proplists:get_value(resource, Config),
    Status = proplists:get_value(status, Config, "undefined"),
    Password = proplists:get_value(password, Config),
    RoomConfs = lists:map(fun(RoomCfg) ->
                                  parse(room_config, RoomCfg)
                          end,
                          proplists:get_value(rooms, Config)),
    Plugins = proplists:get_value(plugins, Config),
    Commands = proplists:get_value(commands, Config),
    jid_config:create(Port, Jid, Status, Resource, Password, RoomConfs, Plugins, Commands);
parse(room_config, {Jid, Config}) ->
    Password = proplists:get_value(password, Config, nopassword),
    Nick = proplists:get_value(nick, Config),
    Commands = proplists:get_value(commands, Config),
    Logging = proplists:get_value(logging, Config),
    Banlist = proplists:get_value(banlist, Config),
    room_config:create(Jid, Nick, Password, Commands, Logging, Banlist).
