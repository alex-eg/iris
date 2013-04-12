-module(tw).
-export([run/1]).

run(Tweet) ->
    ApiConfig = gen_server:call(root, {get_config, twitter_api}),
    ConsumerKey = proplists:get_value(consumer_key, ApiConfig),
    ConsumerSecret = proplists:get_value(consumer_secret, ApiConfig),
    AccessToken = proplists:get_value(access_token, ApiConfig),
    AccessTokenSecret = proplists:get_value(access_token_secret, ApiConfig),
    
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    %%
    RequestTokenURL = "https://api.twitter.com/oauth/request_token",
    {ok, RequestTokenResponse} = oauth:get(RequestTokenURL, [], Consumer),
    RequestTokenParams = oauth:params_decode(RequestTokenResponse),
    RequestToken = oauth:token(RequestTokenParams),
    RequestTokenSecret = oauth:token_secret(RequestTokenParams),
    %%
    URL = "https://api.twitter.com/1.1/statuses/update.json",
	
    {ok, Response} = oauth:get(URL, [{"status", Tweet}], Consumer, AccessToken, AccessTokenSecret),
    ulog:info("Got twitter response: ~p", [Response]),
    oauth:params_decode(Response).
