-module(g).
-export([run/1]).

run("") ->
    "Nothing to search";
run(Args) ->
    [{google_search, SearchConfig}] = gen_server:call(root, {get_config, google_search}),
    
    Query = re:replace(Args, "\s+", "+", [global, unicode, {return, list}]),
    ApiKey = proplists:get_value(api_key, SearchConfig),
    EngineId = proplists:get_value(engine_id, SearchConfig),
    QueryURL = "https://www.googleapis.com/customsearch/v1?key="
    	++ ApiKey
    	++ "&cx=" ++ EngineId
    	++ "&q=" ++ Query
	++ "&num=1",

    ResponseJSON = gen_server:call(root, {get_http, QueryURL}),
    {Response} = jsonx:decode(list_to_binary(ResponseJSON)),
    {<<"items">>, ResultList} = lists:keyfind(<<"items">>, 1, Response),
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
    binary_to_list(Title) ++ "\n" ++
	binary_to_list(Snippet) ++ "\n" ++
	binary_to_list(Link).
    
