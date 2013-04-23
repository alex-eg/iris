-module(word).
-export([run/1]).
-export([shortcut/1]).
-compile(export_all).
-behaviour(iris_module).

run("") ->
    "What is the sound of one hand clapping?";
run(UnescapedArgs) ->
    Args = binary_to_list(unicode:characters_to_binary(UnescapedArgs)),
    [{denshi_jisho, Config}] = gen_server:call(root, {get_config, denshi_jisho}),
    Base = proplists:get_value(request_url, Config),
    [Head|Tail] = string:tokens(Args, " "),
    QueryURL = make_request_url(Head, Tail, Base),
    Response = gen_server:call(root, {get_http, QueryURL}), % Returns only document body
    %% Here be dragons
    Dom = mochiweb_html:parse(Response),
    extract_info(Dom).

shortcut(UnescapedArgs) ->
    Args = binary_to_list(unicode:characters_to_binary(UnescapedArgs)),
    application:start(inets),
    Base = "http://jisho.org/words?jap=~s&eng=~s&dict=edict",
    [Head|Tail] = string:tokens(Args, " "),
    QueryURL = make_request_url(Head, Tail, Base),
    
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Response}} = httpc:request(QueryURL),

    Dom = mochiweb_html:parse(Response),
    extract_info(Dom).

make_request_url("en", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, ["", Query]);
make_request_url("jp", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query, ""]);
make_request_url(Something, Tail, Base) ->	% Defaulting to jp clause
    make_request_url("jp", [Something|Tail], Base).


create_query([]) ->
    "";
create_query([H|[]]) ->
    EscapedQuery = http_uri:encode(H),
    re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]);
create_query([H|T]) ->
    UnescapedQuery = lists:flatten([H|[" " ++ X || X <- T]]),
    EscapedQuery = http_uri:encode(UnescapedQuery),
    re:replace(EscapedQuery, "%20", "+", [global, unicode, {return, list}]).


extract_info({<<"html">>, _, [_Head, Body|_Tail]}) ->
    {<<"body">>, _, BodyTags} = Body,
    FoundWords = proplists:get_value(comment, BodyTags),
    extract_words(FoundWords, BodyTags);
extract_info(_AnythingElse) ->
    "Something mysterious happened!".


extract_words(undefined, _) ->
    "Nothing found";
extract_words(<<" Found words ">>, BodyTags) ->
    KanjiTags = get_tag_list(<<"td">>,
			     {<<"class">>, <<"kanji_column">>},
			     BodyTags),
    Kanji = extract_kanji(lists:reverse(KanjiTags)),
    KanaTags = get_tag_list(<<"td">>,
			{<<"class">>, <<"kana_column">>},
			BodyTags),
    Kana = extract_kana(lists:reverse(KanaTags)),
    MeaningTags = get_tag_list(<<"td">>,
			   {<<"class">>, <<"meanings_column">>},
			   BodyTags),
    Meaning = extract_meaning(lists:reverse(MeaningTags)),
        
    SenseTags = get_tag_list(<<"span">>,
			     {<<"class">>, <<"tags">>},
			     BodyTags),
%%    Sense = extract_sense(SenseTags),
    io:format("KanjiTags:~n~p~nKanji:~n~p~nKana: ~p~nMean: ~p~nTags:~p~n",
	      [KanjiTags, Kanji, KanaTags, Meaning, SenseTags]),
    ok.


-spec get_tag_list(bitstring(),
		   {bitstring(), bitstring()},
		   bitstring()) -> list().
get_tag_list(Name, {Param, Value}, Tree) ->
    collect_tags(Name, {Param, Value}, Tree, []).


-spec collect_tags(bitstring(), 
		   {bitstring(), bitstring()},
		   bitstring(),
		   list()) -> list().
collect_tags(_, _, [], Accum) ->
    Accum;
collect_tags(Name, {Param, Value}, [PCDATA|Tail], Accum) when is_bitstring(PCDATA) -> %% Skip raw text
    collect_tags(Name, {Param, Value}, Tail, Accum);
collect_tags(Name, {Param, Value}, [{comment, _}|Tail], Accum) -> %% Skip comments
    collect_tags(Name, {Param, Value}, Tail, Accum);
collect_tags(Name, {Param, Value}, Tree, Accum) ->
    [H|T] = Tree,
    {HeadName, HeadParams, HeadChildren} = H,
    IsMember = lists:member({Param, Value}, HeadParams),
    case HeadChildren of
	[] ->
	    NewTree = T;
	
	[_NotEmptyList|_] ->
	    NewTree = HeadChildren ++ T
    end,

    if HeadName == Name andalso
       IsMember ->
	    collect_tags(Name, {Param, Value}, NewTree, [H|Accum]);
       not (HeadName == Name) orelse
       not IsMember ->
	    collect_tags(Name, {Param, Value}, NewTree, Accum)
    end.

extract_kanji(KanjiList) ->
    lists:map(fun(Tag) ->
		      {_,_,[Span]} = Tag,
		      {_,_, Children} = Span,
		      Kanji = lists:map(fun get_pcadata/1, Children),
		      lists:foldr(fun(Elem, Acc) ->
					  <<Elem/binary, Acc/binary>>
				  end,
				  <<"">>,
				  Kanji)
	      end,
	      KanjiList).
			
  
extract_kana(KanaList) ->
    lists:map(fun(Tag) ->
		      {_,_,[PCDATA]} = Tag,
		      PCDATA
	      end,
	      KanaList).

extract_meaning(MeaningList) ->
    Meanings = lists:map( 
		 fun(Tag) ->
			 {_,_,Meanings} = Tag,
			 Meanings
		 end,
		 MeaningList),
    Meanings.    
    

get_pcadata(Tag) when is_binary(Tag) ->
    Tag;
get_pcadata([]) ->
    <<"">>;
get_pcadata({Tag, Attributes, [Child]}) when is_bitstring(Tag) ->
    get_pcadata(Child).
    




			  
    
