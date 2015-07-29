IRIS
====

Iris is a jabber bot.

API
---

Iris uses plugins to provide various functionality.
Plugins entry point modules reside in `src/plugins` directory. Each plugin module have to have `-behaviour(iris_plugin).` line at top of the file, and have to implement two callbacks -- `start/3` and `process_message/2`, which are needed to use this behaviour:
```erlang
-callback start(
            Supervisor :: pid(), %% plugin_supervisor pid
            Config :: map(),     %% jid_config map (from data_structures dir)
            From :: pid()) ->    %% jid_worker pid
    term().

-callback process_message(Message :: map(),   %% message map (also of data structures kind)
                          Config :: map()) -> %% jid_config
    term().
```
Each plugin is started and initialized by corresponding jid_worker.
While running, each xmpp message gets passed in each plugin's `process_message` function.

Chat commands
-------------

One of default plugins is chat commands processor.
Each chat command must be stored in separate module and must conform to following interface (implemented through `iris_command` behaviour):
```erlang
-module(iris_command).

-type run_return() :: ok
                    | string().

-callback run(Arguments :: list(string()) %% argument list
                         | [],
              From :: string()) ->        %% jid
    run_return().
```
Example of simple greeter chat command:
```erlang
-module(greet).
-export([run/2]).
-behavior(iris_command).
-alias("@greet").

run(_, From) ->
    [_Room|NickList] = string:tokens(From, "/"),
    Nick = string:join(NickList, "/"),
    "Hello, " ++ Nick.
```
Configuration
-------------

All configuration is stored in priv/iris.config file. Default/example configaration file is provided for your consideration: [default.iris.config](https://github.com/taptap/iris/blob/master/priv/default.iris.config), which is (hopefully) pretty self-explanatory.

Deps
----------------

[jiffy](https://github.com/davisp/jiffy) --- JSON processing

[exmpp](http://processone.github.com/exmpp/) --- XMPP handling

[erlang oauth](https://github.com/tim/erlang-oauth)

[mochiweb_html](https://github.com/mochi/mochiweb) --- HTML parsing

[lager](https://github.com/basho/lager) --- logging
