-module(word).
-export([run/1]).
-export([shortcut/1]).
-compile(export_all).
-behaviour(iris_module).

run("") ->
    "What is the sound of one hand clapping?";
run(Args) ->
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
make_request_url("ja", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query, ""]);
make_request_url(Something, Tail, Base) ->	% Defaulting to jp clause
    make_request_url("ja", [Something|Tail], Base).


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
    "No comments in document, w00t!";
extract_words(<<" Found no words ">>, _) ->
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
    Sense = extract_tags(lists:reverse(SenseTags)),

    KanaStringList = lists:map(fun binary_to_list/1, Kana),
    KanjiStringList = lists:map(fun binary_to_list/1, Kanji),
    MeaningStringList = lists:map(fun binary_to_list/1, Meaning),
    SenseStringList = lists:map(fun binary_to_list/1, Sense),

    Len = min(2, length(KanjiStringList)), % only first two results
    {Start, _} = lists:split(Len,
			     lists:zip(lists:zip(KanjiStringList, KanaStringList), 
				       lists:zip(MeaningStringList, SenseStringList))),
    Result = lists:map(fun({{Kj, Kn}, {Mn, Tg}}) ->
			       Kj1 = string:strip(Kj),
			       Kj2 = string:strip(Kj1, right, $\t),

			       Kn1 = string:strip(Kn),
			       Kn2 = string:strip(Kn1, right, $\t),

			       Mn1 = string:strip(Mn),
			       Mn2 = string:strip(Mn1, right, $\t),
			       io:format("~nBaka:~p~nStr:~p~n", [Mn2, Mn2]),
			       Mn3 = re:replace(Mn2, "; *([1-9]:)", "\n\\1", [unicode, global, {return, list}]),

			       Tg1 = string:strip(Tg),
			       Tg2 = string:strip(Tg1, right, $\t),
			       Tg3 = string:strip(Tg2, right, $\n),

			       lists:flatten(io_lib:format("~n~s [~s]~n~s~n~s~n",[Kj2, Kn2, Mn3, Tg3]))
		       end,
		       Start),
    string:strip(lists:flatten(Result), right, $\n).

-spec get_tag_list(binary(),
		   {binary(), binary()},
		   binary()) -> list().
get_tag_list(Name, {Param, Value}, Tree) ->
    collect_tags(Name, {Param, Value}, Tree, []).


-spec collect_tags(binary(), 
		   {binary(), binary()},
		   binary(),
		   list()) -> list().
collect_tags(_, _, [], Accum) ->
    Accum;
collect_tags(Name, {Param, Value}, [PCDATA|Tail], Accum) when is_binary(PCDATA) -> %% Skip raw text
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
		      {_,_,Children} = Tag,
		      Kana = lists:map(fun get_pcadata/1, Children),
		      lists:foldr(fun(Elem, Acc) ->
					  <<Elem/binary, Acc/binary>>
					      end,
				  <<"">>,
				  Kana)
	      end,
	      KanaList).

extract_meaning(MeaningList) ->
    Meanings = lists:map( 
		 fun(Tag) ->
			 {_,_,Children} = Tag,
			 Meanings = lists:map(fun get_pcadata/1, Children),
			 lists:foldr(fun(Elem, Acc) ->
					     <<Elem/binary, Acc/binary>>
				     end,
				     <<"">>,
				     Meanings)
		 end,
		 MeaningList),
    Meanings.    
    
extract_tags(TagList) ->
    Tags = lists:map(
 	     fun(Tag) ->
 		     {_, _, Children} = Tag,
		     Tags = lists:map(fun get_pcadata/1, Children),
		     lists:foldr(fun(Elem, Acc) ->
					 <<Elem/binary, Acc/binary>>
				 end,
				 <<"">>,
				 Tags)
		 end,
		 TagList),
    Tags.


get_pcadata(Tag) when is_binary(Tag) ->
    Tag;
get_pcadata([]) ->
    <<"">>;
get_pcadata({Tag, _Attributes, []}) when is_bitstring(Tag) ->
    <<"">>;
get_pcadata({Tag, _Attributes, [Child]}) when is_bitstring(Tag) ->
    get_pcadata(Child).			  
