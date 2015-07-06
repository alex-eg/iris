-module(word).
-export([run/2]).
-behaviour(iris_command).
-alias("@word").

run([], _) ->
    "What is the sound of one hand clapping?";
run(Args, _) ->
    [{request_url, Base}] = jid_worker:get_config(denshi_jisho),
    [Head|Tail] = Args,
    QueryURL = make_request_url(Head, Tail, Base) ++ "%23words", % for #words, to tell jisho we are searching words
    {{_, 200, _}, _, Response} = misc:httpc_request(get, {QueryURL, []}, [], []),
    Dom = mochiweb_html:parse(Response),
    %% 2 because why not
    lists:flatten(lists:sublist(extract_info(Dom), 2)).

make_request_url("en", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query, ""]);
make_request_url("ja", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query, ""]);
make_request_url(Something, Tail, Base) -> % Defaulting to jp clause
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


extract_info(Dom) ->
    Furi        = mochiweb_xpath:execute("//div[@class='concept_light-representation']/span[@class='furigana']", Dom),
    Text        = mochiweb_xpath:execute("//div[@class='concept_light-representation']/span[@class='text']", Dom),
    MeaningTags = mochiweb_xpath:execute("//div[@class='meanings-wrapper']", Dom),
    MaxLen = min(min(length(Furi),
                     length(Text)),
                 length(MeaningTags)),
    lists:map(
      fun({TextE, FuriE, MeaningE}) ->
              Spelling = lists:flatten(get_kanji(TextE)),
              Reading = lists:flatten(reading(get_furi(FuriE),
                                              (get_okuri(TextE)))),
              Meanings = lists:takewhile(
                           fun({tag, Tag}) ->
                                   not(string:equal(Tag, "Wikipedia definition")
                                       or string:equal(Tag, "Other forms") 
                                       or string:equal(Tag, "Notes"));
                              (_) -> true
                           end,
                           get_meanings(MeaningE)),
              MeaningText = lists:flatten(
                              begin
                                  {_, MeaningListText} = 
                                      lists:foldl(
                                        fun({tag, Tag}, {_, Acc}) ->
                                                {1, Acc ++ io_lib:format("(~s): ", [Tag])};
                                           ({meanings, MeaningEntry}, {EntryNum, Acc}) ->
                                                {EntryNum + 1, Acc ++ io_lib:format("~w. ~s~n", [EntryNum, MeaningEntry])}
                                        end,
                                        {1, ""},
                                        Meanings),
                                  MeaningListText
                              end),
              io_lib:format("~n~s [~s]~n~s", [Spelling, Reading, MeaningText])
      end,
      lists:zip3(lists:sublist(Text, MaxLen),
                 lists:sublist(Furi, MaxLen),
                 lists:sublist(MeaningTags, MaxLen))).
      

get_meanings({<<"div">>, [{<<"class">>,<<"meanings-wrapper">>}], Contents}) ->
    lists:map(fun(C = {<<"div">>,
                       [{<<"class">>,<<"meaning-wrapper">>}],
                       _}) ->
                      {meanings, collect_meanings(C)};
                 ({<<"div">>,
                   [{<<"class">>,<<"meaning-tags">>}],
                   [Tag]}) ->
                      {tag, convert(Tag)}
              end,
              Contents).
                      
collect_meanings(Contents) ->
    Meanings = mochiweb_xpath:execute("//span[@class='meaning-meaning']", Contents),
    lists:flatten(
      lists:map(fun({<<"span">>,
                     [{<<"class">>,<<"meaning-meaning">>}],
                     Meaning}) ->
                        lists:map(fun (M) ->
                                          case M of
                                              {<<"span">>, _, _} ->
                                                  no;
                                              MeaningText -> convert(MeaningText)
                                          end
                                  end,
                                  Meaning)
                end,
                Meanings)).

reading(Furi, Okuri) ->
    io_lib:format(lists:flatten(Furi), Okuri).

get_furi({<<"span">>, [{<<"class">>,<<"furigana">>}], Contents}) ->
    lists:map(fun(C) ->
                      {<<"span">>, _, Text} = C,
                      case Text of
                          [] -> "~s";
                          [Kana] -> convert(Kana)
                      end
              end,
              Contents).

get_okuri({<<"span">>, [{<<"class">>,<<"text">>}], Contents}) ->
    lists:foldr(fun(C, A) ->
                        %% Good exapmle:
                        %% <span class="text">
                        %%     知<span>り</span>合<span>い</span>
                        %% </span>
                        case C of
                            {<<"span">>, _, [Okuri]} -> [convert(Okuri)|A];
                            _ -> A
                        end
		end,
		[],
		Contents).
    

get_kanji({<<"span">>, [{<<"class">>,<<"text">>}], Contents}) ->
    lists:map(fun(Text) ->
                      %% Good exapmle:
                      %% <span class="text">
                      %%     知<span>り</span>合<span>い</span>
                      %% </span>
                      case Text of
                          {<<"span">>, [], [Okurigana]} -> convert(Okurigana);
                          Kanji -> convert(Kanji)
                      end
              end,
              Contents).

convert(BitString) ->
    string:strip(string:strip(string:strip(binary_to_list(BitString)),
                 both, $\n)).
