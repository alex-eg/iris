-include_lib("exmpp/include/exmpp.hrl").

-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
-define(REJOIN_TIMEOUT, 30000). 
-define(DEFAULT_COMMAND_PREFIX, "@").
-define(DEFAULT_CONFIG_FILE, "priv/cfg.erl").
