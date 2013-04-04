-module(muc_tools).
-export([join_groupchat/2, send_muc_keepalive/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

join_groupchat(Session, RoomTuple) ->
    {Room, Nick, Password} = RoomTuple,
    ulog:info("Joining ~s as ~s", [Room, Nick]),
    Presence = create_presence(Room, Nick, Password),
    exmpp_session:send_packet(Session, Presence).

send_muc_keepalive(Session, RoomTuple) ->
    {Room, Nick, Password} = RoomTuple,
    ulog:debug("Processing muc keepalive for room ~s", [Room]),
    Presence = create_presence(Room, Nick, Password),
    exmpp_session:send_packet(Session, Presence).
    
%% Helpers below
create_presence(Room, Nick, nopassword) ->
    %% Fixme: self-defined marco and manual presence creation is definetly NOT a good practice
    %% Well, exmpp seems to provide no such functionality, so I have to prettify it by myself
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE,
					   <<"to">>,
					   list_to_binary(Room ++ "/" ++ Nick)),
    exmpp_xml:append_child(BasePresence,
			   #xmlel{name = x, 
				  attrs = [#xmlattr{name = <<"xmlns">>, 
						    value = ?NS_MUC_b}]
				 }
			  );
create_presence(Room, Nick, Password) ->
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE,
					   <<"to">>,
					   list_to_binary(Room ++ "/" ++ Nick)),
    PasswordElement = #xmlel{name = password,
		      children = [#xmlcdata{cdata = Password}]},
    exmpp_xml:append_child(BasePresence,
			   #xmlel{name = x, 
				  attrs = [#xmlattr{name = <<"xmlns">>, 
						    value = ?NS_MUC_b}],
				  children = [PasswordElement]
				 }
			  ).
    
