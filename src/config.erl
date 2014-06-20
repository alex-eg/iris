-module(config).
-export([init/1, get_room_list/1, parse/2]).

-include("xmpp.hrl").

init(Filename) ->
    {ok, [ConfigList]} = file:consult(Filename),
    ulog:info("Read configuration file ~s", [Filename]),
    ConfigList.

parse(jid_config, {jid_config, Config}) ->
    Jid = proplists:get_value(jid, Config),
    Port = proplists:get_value(port, Config, 5222),
    Resource = proplists:get_value(resource, Config),
    Status = proplists:get_value(status, Config, "undefined"),
    Password = proplists:get_value(password, Config),
    RoomConfs = lists:map(fun(RoomCfg) ->
                                  parse(room_config, RoomCfg)
                          end,
                          proplists:get_value(rooms, Config)),
    Modules = proplists:get_value(modules, Config),
    jid_config:create(Port, Jid, Status, Resource, Password, RoomConfs, Modules).

parse(room_config, {Jid, Config}) ->
    Nick = proplists:get_value(nick, Config),
    Modules = proplists:get_value(modules, Config),
    Logging = proplists:get_value(logging, Config),
    Banlist = proplists:get_value(banlist, Config),
    room_config:create(Jid, Nick, Modules, Logging, Banlist).
