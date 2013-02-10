-include_lib("exmpp/include/exmpp.hrl").

-record(bot_info,
	{port = 5222,
	 server_address,
	 jid,
	 password,
	 room,
	 nick,
	 status,
	 timeout = 10000,
	 modules = []
	}).

-record(connection_info,
	{port = 5222,
	 jid,
	 password,
	 status,
	 resource,
	 modules = []
	 }).

-record(room_info,
	{jid,
	 password = none,
	 nick,
	 modules = []
	 }).


-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
