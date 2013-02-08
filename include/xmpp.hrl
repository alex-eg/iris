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

-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
