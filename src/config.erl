-module(config).
-export([init/1, get_room_list/1, parse/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

init(Filename) ->
    {ok, [ConfigList]} = file:consult(Filename),
    ulog:info("Read configuration file ~s", [Filename]),
    ConfigList.

parse(jid_config, Config) ->
    %% Debug only
    %% {value, {resource, Resource}} = lists:keysearch(resource, 1, Config),
    {_, Resource} = init:script_id(),
    #jid_info {
       jid = proplists:get_value(jid, Config),
       resource = proplists:get_value(resource, Config),
       status = proplists:get_value(status, Config),
       password = proplists:get_value(password, Config),
       rooms = proplists:get_value(rooms, Config),
       modules = proplists:get_value(modules, Config)
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
	      
	      
	  
			  
