-module(shrink).
-export([run/2]).
-behavior(iris_command).
-alias("@shrink").

run([], _) ->
    "Nothing to shrink-shrink about!";

run(Args, _) ->
    QueryURL = string:join(Args, " "),
    EncodedQueryURL = http_uri:encode(QueryURL),
    EncodedQueryURL,
    {{_, 200, _}, _, ResponseJSON} = misc:httpc_request(post, {"http://api.xn--jj0a.jp/generate.json", [{"User-Agent", "iris/1.0"}],
                                                               "application/x-www-form-urlencoded", "fUrl=" ++ EncodedQueryURL ++ "&id="}, [], []),
    parse(ResponseJSON).

parse(ResponseJSON) ->
    case jiffy:decode(list_to_binary(ResponseJSON)) of
        {[{<<"result">>, {Response}}]} -> {_, Result} = lists:keyfind(<<"generated">>, 1, Response),
            binary_to_list(Result);
        {[{<<"error">>, Response}]} -> Response
    end.
