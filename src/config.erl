-module(config).
-export([init/1, get_room_list/1]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

init(Filename) ->
    {ok, [{jid_config, Config}]} = file:consult(Filename),
    {value, {jid, Jid}} = lists:keysearch(jid, 1, Config),
    %% Debug only
    %% {value, {resource, Resource}} = lists:keysearch(resource, 1, Config),
    {_, Resource} = init:script_id(),
    {value, {status, Status}} = lists:keysearch(status, 1, Config),
    {value, {password, Password}} = lists:keysearch(password, 1, Config),
    {value, {rooms, Rooms}} = lists:keysearch(rooms, 1, Config),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Config),
    ulog:info("Read configuration file ~s", [Filename]),
    #jid_info {
       jid = Jid,
       resource = Resource,
       status = Status,
       password = Password,
       rooms = Rooms,
       modules = Modules
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
	      
	      
	  
			  
