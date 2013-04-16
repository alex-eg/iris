-module(word).
-export([run/1]).
-behaviour(iris_module).

run(Args) ->
    [{denshi_jisho, Config}] = gen_server:call(root, {get_config, denshi_jisho}),
    Base = proplists:get_value(request_url, Config),
    [Head|Tail] = string:tokens(Args, " "),
    QueryURL = make_request_url(Head, Tail, Base),
    Response = gen_server:call(root, {get_http, QueryURL}),
    %% Here be dragons
    

make_request_url("en", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, ["", Query]);
make_request_url("jp", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query, ""]);
make_request_url(Something, Tail, Base) ->	% Defaulting to jp clause
    Query = Something ++ " " ++ create_query(Tail, Base),
    io_lib:format(Base, [Query, ""]).

create_query([]) ->
    "";
create_query([H|[]]) ->
    Escaped = http_uri:encode(H),
    re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]);
create_query([H|T]) ->
    UnescapedQuery = lists:flatten([H|[" " ++ X || X <- T]]),
    EscapedQuery = http_uri:encode(UnescapedQuery),
    re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]).
    
    
    
    
    
