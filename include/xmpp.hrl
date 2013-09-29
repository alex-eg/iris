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

-record(bot_info,
        {google_search = undefined
        }).

-define(EMPTY_PRESENCE, #xmlel{name = 'presence'}).
-define(REJOIN_TIMEOUT, 30000). 
-define(COMMAND_PREFIX, "@").
-define(QUEUE_SIZE, 10). % store last 10 messages per peer
-define(CONFIG_FILE, "priv/cfg.erl").
-define(LOG_DIR, "logs").
