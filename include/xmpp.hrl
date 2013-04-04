-include_lib("exmpp/include/exmpp.hrl").

-record(jid_info,
	{port = 5222,
	 jid,
	 resource,
	 status,
	 password,
	 rooms = [],
	 modules = []
	}).

-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
-define(RESTART_TIMEOUT, 5000). %% time in miliseconds
-define(REJOIN_TIMEOUT, 30000). 
-define(COMMAND_PREFIX, "!").
