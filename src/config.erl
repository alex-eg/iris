-module(config).
-export([init/1, get_room_list/1, parse/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

init(Filename) ->
    {ok, [ConfigList]} = file:consult(Filename),
    ulog:info("Read configuration file ~s", [Filename]),
    ConfigList.

parse(jid_config, Config) ->
    {value, {jid, Jid}} = lists:keysearch(jid, 1, Config),
    %% Debug only
    %% {value, {resource, Resource}} = lists:keysearch(resource, 1, Config),
    {_, Resource} = init:script_id(),
    Status = proplists:get_value(status, Config),
%%    {value, {status, Status}} = lists:keysearch(status, 1, Config),
    {value, {password, Password}} = lists:keysearch(password, 1, Config),
    {value, {rooms, Rooms}} = lists:keysearch(rooms, 1, Config),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Config),
    #jid_info {
       jid = Jid,
       resource = Resource,
       status = Status,
       password = Password,
       rooms = Rooms,
       modules = Modules
      };
parse(bot_config, Config) ->
    ApiKey = proplists:get_value(api_key, Config),
    EngineId = proplists:get_value(engine_id, Config),
    #bot_info{
       api_key = ApiKey,
       engine_id = EngineId
      }.
    
get_room_list(#jid_info{rooms = RoomList}) ->
    lists:map(
      fun(RoomTuple) ->
	      if tuple_size(RoomTuple) == 2 ->
		      {Room, Nick} = RoomTuple,
		      {Room, Nick, nopassword};
		 tuple_size(RoomTuple) == 3 ->
		      {Room, Nick, Password} = RoomTuple,
		      {Room, Nick, Password};
		 true -> 
		      ulog:error("Bad Room Tuple ~p", [RoomTuple]),
		      error
	      end
      end,
      RoomList).
	      
	      
	  
			  
