-module(room_config).
-export([create/5, jid/1, nick/1, commands/1, logging/1, banlist/1]).

create(Jid, Nick, Commands, Logging, Banlist) ->
    #{jid => Jid,
      nick => Nick,
      commands => Commands,
      logging => Logging,
      banlist => Banlist}.

jid(Config) ->
    maps:get(jid, Config).
           
nick(Config) ->
    maps:get(nick, Config).

commands(Config) ->
    maps:get(commands, Config).

logging(Config) ->
    maps:get(logging, Config).

banlist(Config) ->
    maps:get(banlist, Config).
