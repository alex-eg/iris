-module(tr).
-export([run/2]).
-behavior(iris_command).
-alias("@tr").

run([], _) ->
    "No. It won't work";
run(ArgList, _) ->
    translate(ArgList, "ru").

translate(ArgList, Direction) ->
    What = string:join(ArgList, " "),
    TranslateConfig = jid_worker:get_config(yandex_translate),
    EscapedQuery = http_uri:encode(What),
    Query = re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]),
    ApiKey = proplists:get_value(api_key, TranslateConfig),
    QueryURL = "https://translate.yandex.net/api/v1.5/tr.json/translate?key="
        ++ ApiKey
        ++ "&text=" ++ Query
        ++ "&lang=" ++ Direction,
    {{_, _, _}, _, ResponseJSON} = misc:httpc_request(get, {QueryURL, []}, [], []),
    {Response} = jiffy:decode(list_to_binary(ResponseJSON)),
    lager:debug("translate retuned: ~p", [Response]),
    {<<"code">>, ResultCode} = lists:keyfind(<<"code">>, 1, Response),
    extract_result(ResultCode, Response).

extract_result(200, Response) ->
    {<<"text">>, [TranslatedText]} = lists:keyfind(<<"text">>, 1, Response),
    binary_to_list(TranslatedText);
extract_result(422, _) ->
    "Cannot translate this for some reason";
extract_result(Other, _) ->
    misc:format_str("Got unknown return code: ~p", [Other]).


