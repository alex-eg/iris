-include_lib("exmpp/include/exmpp.hrl").

-record(jid_info,
	{port = 5222,
	 server_address,
	 jid,
	 password,
	 room,
	 nick,
	 resource,
	 status,
	 timeout = 10000,
	 modules = [],
	 roster = []
	}).

-record(connection_info,
	{port = 5222,
	 jid,
	 password,
	 status,
	 modules = []
	}).

-record(room_info,
	{jid,
	 password = none,
	 nick,
	 modules = []
	}).


-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
