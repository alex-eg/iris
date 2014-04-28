IRIS
====

Iris is a ~~scalable jabber bot.~~ a bunch of crap. Disregard this readme.

JSON processing by [iskra's jsonx module](https://github.com/iskra/jsonx)

XMPP handling by [excellent Process One's library](http://processone.github.com/exmpp/)

OAuth protocol library by [erlang oauth](https://github.com/tim/erlang-oauth)

API
---

Modules must conform to iris_module behaviour:
```erlang
-module(default_module).
-export([run/2]).
-behaviour(iris_module).

run(ArgumentString, SenderJidWithResource) ->
    ok.
```

Iris' modules are text-processing modules, and are invoked when the bot recieves message starting with COMMAND_PREFIX character, defined in [xmpp.hrl file](https://github.com/taptap/iris/blob/master/include/xmpp.hrl). The only `run/2` command recieves the rest of the message (excluding prefix and command name) or an empty string, and sender's JID with resource (which stands for nick in case of MUCs), for example: 

        alice@example.com/microwave_oven

        roomname@conference.example.com/Romeo
        roomname@conference.example.com/Juliet

Each parameter, which may be needed by the module, can be stored into main config file in a standart key-value way. Inside the module, you can access the parameter using

     	gen_server:call(root, {get_config, Key}).

It's recommended to store one global term and handle it inside the module.

Also you can perform http and https GETs using the following command, which returns list, containing request URL, or an `error` atom:

     	Response = gen_server:call(root, {get_http, QueryURL})

Configuration
-------------

All configuration is stored in priv/cfg.erl file. Default/example configaration file is provided for your consideration: [default.cfg.erl](https://github.com/taptap/iris/blob/master/priv/default.cfg.erl), which is (hopefully) pretty self-explanatory.

Each module and each plugin store it's configuration parameters inside cfg.erl file.

So, cfg.erl is a single list, containig key-value pairs, that is, actually a proplist.

Plugins
-------

Plugins are standalone processes, spawn for each jid worker which extend bot's functionality. 
By now there is only one plugin (external interface), but some more are planned. Plugin API is believed to be based on hooks.