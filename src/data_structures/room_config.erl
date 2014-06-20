-module(room_config).
-export(create/5, jid/1, nick/1, modules/1, logging/1, banlist/1).

create(Jid, Nick, Modules, Logging, Banlist) ->
    #{jid => Jid,
      nick => Nick,
      modules => Modules,
      logging => Logging,
      banlist => Banlist}.

jid(Config) ->
    maps:get(jid, Config).
           
nick(Config) ->
    maps:get(nick, Config).

modules(Config) ->
    maps:get(modules, Config).

logging(Config) ->
    maps:get(logging, Config).

banlist(Config) ->
    maps:get(banlist, Config).
