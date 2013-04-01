-module(muc_tools).
-export([join_groupchat/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").

join_groupchat(Session, RoomTuple) ->
    {Room, Nick, Password} = RoomTuple,
    join_groupchat(Session, Room, Nick, Password).

join_groupchat(Session, Room, Nick, nopassword) ->
    ulog:info("Joining ~s as ~s", [Room, Nick]),
    Presence = create_basic_presence(Room, Nick),
    exmpp_session:send_packet(Session, Presence);
join_groupchat(Session, Room, Nick, Password) ->
    BasePresence = create_basic_presence(Room, Nick),
    Presence = exmpp_xml:append_child(BasePresence#xmlel{name = x},
				      #xmlel{name = password,
					     children = [#xmlcdata{cdata = Password}]}),
    exmpp_session:send_packet(Session, Presence).
    
%% Helpers below
create_basic_presence(Room, Nick) ->
    %% Fixme: self-defined marco and manual presence creation is definetly NOT a good practice
    %% Well, exmpp seems to provide no such functionality, so I have to prettify it by myself
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE,
					   <<"to">>,
					   list_to_binary(Room ++ "/" ++ Nick)),
    Presence = exmpp_xml:append_child(BasePresence,
				      #xmlel{name = x, 
					     attrs = [#xmlattr{name = <<"xmlns">>, 
							       value = ?NS_MUC_b}]
					    }
				     ).


    
