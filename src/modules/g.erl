-module(g).
-export([run/2]).
-behavior(iris_module).

run("", _) ->
    "Nothing to search";
run(Args, _) ->
    [{google_search, SearchConfig}] = gen_server:call(core, {get_config, google_search}),

    EscapedQuery = http_uri:encode(Args),
    Query = re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]),
    ApiKey = proplists:get_value(api_key, SearchConfig),
    EngineId = proplists:get_value(engine_id, SearchConfig),
    QueryURL = "https://www.googleapis.com/customsearch/v1?key="
        ++ ApiKey
        ++ "&cx=" ++ EngineId
        ++ "&q=" ++ Query
        ++ "&num=1",    % We need only 1st result
    {{_, 200, _}, _, ResponseJSON} = gen_server:call(core, {get_http, get, {QueryURL, []}, [], []}),
    {Response} = jsonx:decode(list_to_binary(ResponseJSON)),
    Items = lists:keyfind(<<"items">>, 1, Response),
    Result = extract_result(Items),
    Result.

extract_result({<<"items">>, ResultList}) ->
    [{FirstResult}|_] = ResultList,
    ResultProplist = lists:map(fun(Entry) ->
                                       {Key, Value} = Entry,
                                       AtomKey = list_to_atom(
                                                   bitstring_to_list(Key)),
                                       {AtomKey, Value}
                               end,
                               FirstResult),
    Title = proplists:get_value(title, ResultProplist),
    Snippet = proplists:get_value(snippet, ResultProplist),
    Link = proplists:get_value(link, ResultProplist),
    ulog:debug("Title:   ~p", [Title]),
    ulog:debug("Snippet: ~p", [Snippet]),
    ulog:debug("Link:    ~p", [Link]),
    Result = binary_to_list(Title) ++ "\n" ++
        binary_to_list(Snippet) ++ "\n" ++
        binary_to_list(Link),
    Result;
extract_result(false) ->
    "Sorry, nothing found".
