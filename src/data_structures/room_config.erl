-module(room_config).
-export([create/6, jid/1, nick/1, commands/1, logging/1, banlist/1, password/1]).

create(Jid, Nick, Password, Commands, Logging, Banlist) ->
    #{jid => Jid,
      nick => Nick,
      password => Password, 
      commands => Commands,
      logging => Logging,
      banlist => Banlist}.

jid(Config) ->
    maps:get(jid, Config).
           
nick(Config) ->
    maps:get(nick, Config).

password(Config) ->
    maps:get(password, Config).

commands(Config) ->
    maps:get(commands, Config).

logging(Config) ->
    maps:get(logging, Config).

banlist(Config) ->
    maps:get(banlist, Config).
