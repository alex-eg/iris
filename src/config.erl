-module(config).
-export([parse/2]).

-include("xmpp.hrl").

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
    OtherConfig = lists:filter(fun({Name, _Entry}) ->
                                       not lists:member(Name,
                                                        [port,
                                                         resource,
                                                         status,
                                                         password,
                                                         rooms,
                                                         plugins])
                               end,
                               Config),
    jid_config:create(Port, Jid, Status, Resource, Password, RoomConfs, Plugins, OtherConfig);
parse(room_config, {Jid, Config}) ->
    Password = proplists:get_value(password, Config, nopassword),
    Nick = proplists:get_value(nick, Config),
    Commands = proplists:get_value(commands, Config),
    Logging = proplists:get_value(logging, Config),
    Banlist = proplists:get_value(banlist, Config),
    room_config:create(Jid, Nick, Password, Commands, Logging, Banlist).
