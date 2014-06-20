-module(shrink).
-export([run/2]).
-behaviour(iris_command).

run("", _) ->
    "Nothing to shrink-shrink about!";

run(Args, _) ->
    [{shrinker, Config}] = gen_server:call(core, {get_config, shrinker}),
    Base = proplists:get_value(request_url, Config),
    Params = proplists:get_value(params, Config),
    Tokens = string:tokens(Args, " "),
    parse(Tokens, Base, Params).

parse([URL], Base, Params) ->
    EncodedURL = http_uri:encode(URL),
    {{_, 200, _}, _, ResponseJSON} = gen_server:call(core, {get_http,
                                                            post,
                                                            {Base, [{"User-Agent", "Maribel/1.0"}],
                                                             "application/x-www-form-urlencoded",
                                                             "fUrl=" ++ EncodedURL ++ "&id="},
                                                            [], []}),
    {[{<<"result">>, {Response}}]} = jiffy:decode(list_to_binary(ResponseJSON)),
    {_, Result} = lists:keyfind(<<"generated">>, 1, Response),
    binary_to_list(Result).

