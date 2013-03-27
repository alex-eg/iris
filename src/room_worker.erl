-module(room_worker.erl).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include("xmpp.hrl").
-behavior(gen_server).

init(State) ->
    [Config, Session] = State,
    Room = Config#room_info.room,
    Nick = Config#room_info.nick,
    Password = Config#room_info.password,
    join_groupchat(Session, Room, Nick, Password),
    {ok, [State]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% gen_server callbacks end

join_groupchat(XmppSession, Room, Nick) ->
    ulog:info("Joining ~s as ~s", [Room, Nick]),
    %% Fixme: self-defined marco and manual presence creation is definetly NOT a good practice
    %% Well, exmpp seems to provide no such functionality, so have to prettify it by myself
    BasePresence = exmpp_xml:set_attribute(?EMPTY_PRESENCE, <<"to">>, list_to_binary(Room ++ "/" ++ Nick)),
    Presence = exmpp_xml:append_child(BasePresence,
				      #xmlel{name = x, attrs = [#xmlattr{name = <<"xmlns">>, value = ?NS_MUC_b}]
					    }
				     ),
    exmpp_session:send_packet(XmppSession, Presence).


    
