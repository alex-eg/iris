-module(tw).
-export([run/2]).
-behavior(iris_command).

run(["@tw"], _) ->
    "A hollow voice says, 'Fool'";
run(["@tw"|ArgList], _) ->
    Tweet = string:join(ArgList, " "),
    [{twitter_api, ApiConfig}] = core:get_config(twitter_api),
    ConsumerKey = proplists:get_value(consumer_key, ApiConfig),
    ConsumerSecret = proplists:get_value(consumer_secret, ApiConfig),
    AccessToken = proplists:get_value(access_token, ApiConfig),
    AccessTokenSecret = proplists:get_value(access_token_secret, ApiConfig),

    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},

    URL = "https://api.twitter.com/1.1/statuses/update.json",

    case oauth:post(URL, [{"status", Tweet}], Consumer, AccessToken, AccessTokenSecret) of
        {ok, _Response} ->
            Tweet;
        {errors, Error} ->
            lager:error("Twitting failed with error: ~p", [Error]),
            "Something went wrong"
    end;
run(_, _) ->
    nope.

