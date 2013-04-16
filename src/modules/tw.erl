-module(tw).
-export([run/1]).
-behavior(iris_module).

run("") ->
    "A hollow voice says, 'Fool'";
run(Twit) ->
    [{twitter_api, ApiConfig}] = gen_server:call(root, {get_config, twitter_api}),
    ConsumerKey = proplists:get_value(consumer_key, ApiConfig),
    ConsumerSecret = proplists:get_value(consumer_secret, ApiConfig),
    AccessToken = proplists:get_value(access_token, ApiConfig),
    AccessTokenSecret = proplists:get_value(access_token_secret, ApiConfig),
    
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},

    URL = "https://api.twitter.com/1.1/statuses/update.json",

    case oauth:post(URL, [{"status", Twit}], Consumer, AccessToken, AccessTokenSecret) of
	{ok, _Response} ->
	    Twit;
	{errors, Error} ->
	    ulog:info("Twitting failed with error: ~p", [Error]),
	    "Something went wrong"
    end.
