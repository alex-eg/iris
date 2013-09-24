-module(shrink).
-export([run/1]).
-behaviour(iris_module).

run("") ->
	"Nothing to shrink-shrink about!";

run(Args) ->
	[{shrinker, Config}] = gen_server:call(core, {get_config, shrinker}),
	Base = proplists:get_value(request_url, Config),
	Params = proplists:get_value(params, Config),
	[URL, CustomID] = string:tokens(Args, " "),
	EncodedURL = http_uri:encode(URL),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Response}} = httpc:request(post, {Base, [{"User-Agent", "curl/7.32.0"}],  "application/x-www-form-urlencoded", io_lib:format(Params, [EncodedURL, CustomID])}, [], []),
	Dom = mochiweb_html:parse(Response).
	%extract_info(Dom).
