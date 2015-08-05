-module(iris_plugin).
-callback start(
            Supervisor :: pid(), %% plugin_supervisor pid
            Config :: map(),     %% jid_config map (from data_structures dir)
            From :: pid()) ->    %% jid_worker pid
    term().

-callback process_message(Message :: map(),     %% message map (also of data structures kind)
                          Config  :: map()      %% jid_config
                          ) ->
    term().

-callback stop(From :: pid()                    %% jid worker pid
                       ) ->
    term().
