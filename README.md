IRIS
====

Iris is a toy jabber bot.

Deps and credits
----------------

JSON processing by [jiffy](https://github.com/davisp/jiffy)

XMPP handling by [excellent Process One's library](http://processone.github.com/exmpp/)

OAuth protocol library by [erlang oauth](https://github.com/tim/erlang-oauth)

HTML parsing by [mochiweb html parser](https://github.com/mochi/mochiweb)

Logging by [lager](https://github.com/basho/lager)

API
---

Iris uses plugin system for extending functionality to be able to perform a lrga variety of tasks.
Plugins entry point modules reside in `src/plugins` directory. Each plugin module have to have `-behaviour(iris_plugin).` line at top of the file, and of course have to implement two callbacks -- `start/3` and `process_message/2`, which are needed in order to use this behaviour:
```erlang
-module(iris_plugin).
-callback start(Parent :: pid(),   %% supervisor process pid
                Config :: map(),   %% jid_config map (from data_structures dir)
                From :: pid()) ->  %% jid_worker pid
    term().

-callback process_message(Message :: map(),   %% message map (also of data structures kind)
                          Config :: map()) -> %% jid_config
    term().
```
Each plugin is started and initialized by corresponding jid_worker.
While running, each xmpp message gets passed in each plugin's `process_message` function.

Chat commands
-------------

Each chat command must be stored in separate module and must conform to following interface (implemented through `iris_command` behaviour):
```erlang
-module(iris_command).

-type run_return() :: nope
                    | string().

-callback run(Arguments :: list(string())  %% message body, split by space character
                         | [],
              From :: string()) ->         %% jid@server/resource string
    run_return().
```
*Important note:* each module must have default clause for `run` function, returning atom `nope`, like this:
```erlang
run(_, _) -> nope.
```
Example of simple greeter chat command:
```erlang
-module(greet).
-export([run/2]).
-behavior(iris_command).

run(["@greet"], From) ->
    [_Room|NickList] = string:tokens(From, "/"),
    Nick = string:join(NickList, "/"),
    "Hello, " ++ Nick;
run(["@greet"|ArgList], _) ->
    "Hello, " ++ string:join(ArgList, " ");
run(_, _) -> nope.
```
Configuration
-------------

All configuration is stored in priv/cfg.erl file. Default/example configaration file is provided for your consideration: [default.cfg.erl](https://github.com/taptap/iris/blob/master/priv/default.cfg.erl), which is (hopefully) pretty self-explanatory.

So, cfg.erl is a single list, containig key-value pairs, that is, actually a proplist.

Global config storage
---------------------
Each parameter, which may be needed by any plugin or chat command, can be stored into main config file in a standart key-value way. Inside the module, you can access the parameter using

     	gen_server:call(root, {get_config, Key}).

It's recommended to store one global term and handle it inside the module.