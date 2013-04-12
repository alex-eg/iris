IRIS
====

Iris is a scalable jabber bot.

JSON processing by [iskra's jsonx module](https://github.com/iskra/jsonx)

XMPP handling by [excellent Process One's library](http://processone.github.com/exmpp/)

OAuth protocol library by [erlang oauth](https://github.com/tim/erlang-oauth)

API
---

Modules must conform to the following template (which will eventually evolve into behaviour):

```erlang
-module(default_module).
-export([run/1]).

run(ArgumentString) ->
    ok.
```

Each parameter, which may be needed by the module, can be stored into main config file in a standart key-value way. Inside the module, you can access the parameter using

     	gen_server:call(root, {get_config, Key}).

It's recommended to store one global term and handle it inside the module.

Also you can perform http and https GETs using the following command, which returns list, containing request URL, or an `error` atom:

     	Response = gen_server:call(root, {get_http, QueryURL})