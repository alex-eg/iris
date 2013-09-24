-module(shrink).
-export([run/2]).
-behaviour(iris_module).

run("", _) ->
    "Nothing to shrink-shrink about!";

run(Args, _) ->
    [{shrinker, Config}] = gen_server:call(core, {get_config, shrinker}),
    Base = proplists:get_value(request_url, Config),
    Params = proplists:get_value(params, Config),
    [URL, CustomID] = string:tokens(Args, " "),
    EncodedURL = http_uri:encode(URL),
    {{_, 200, _}, _, Response} = gen_server:call(core, {get_http, 
                                                        post, 
                                                        {Base, [{"User-Agent", "curl/7.32.0"}], 
                                                         "application/x-www-form-urlencoded", 
                                                         io_lib:format(Params, [EncodedURL, CustomID])}, 
                                                        [], []}),
    Dom = mochiweb_html:parse(Response),
    "nipah~".
