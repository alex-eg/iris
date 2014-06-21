-module(jid_config).
-export([create/7]).
-export([port/1, jid/1, resource/1, status/1, password/1, room_confs/1, modules/1]).

create(Port, Jid, Status, Resource, Password, RoomConfs, Modules) ->
    #{port => Port,
      jid => Jid,
      status => Status,
      resource => Resource,
      password => Password,
      room_confs => RoomConfs,
      modules => Modules}.

port(State) ->
    maps:get(port, State).

jid(State) ->
    maps:get(jid, State).

resource(State) ->
    maps:get(resource, State).

status(State) ->
    maps:get(status, State).

password(State) ->
    maps:get(password, State).

room_confs(State) ->
    maps:get(room_confs, State).

modules(State) ->
    maps:get(modules, State).
