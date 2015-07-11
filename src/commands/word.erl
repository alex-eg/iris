-module(word).
-export([run/2]).
-behaviour(iris_command).
-alias("@word").
-compile(export_all).
run([], _) ->
    "What is the sound of one hand clapping?";
run(Args, _) ->
    [{request_url, Base}] = jid_worker:get_config(denshi_jisho),
    QueryURL = make_request_url(Args, Base) ++ "%23words", % for #words, to tell jisho we are searching words
    {{_, 200, _}, _, Response} = misc:httpc_request(get, {QueryURL, []}, [], []),
    Dom = mochiweb_html:parse(Response),
    %% 2 because why not
    Reply = string:strip(lists:flatten(lists:sublist(extract_info(Dom), 2)),
                         right,
                         $\n),
    case string:equal(Reply, "") of
        true ->"Nothing found";
        false -> Reply
    end.

make_request_url(Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, [Query]).

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
                                              get_okuri(TextE))),
              Meanings = lists:takewhile(
                           fun({tag, Tag}) ->
                                   not(string:equal(Tag, "Other forms")
                                       or string:equal(Tag, "Notes"));
                              (_) -> true
                           end,
                           get_meanings(MeaningE)),
              MeaningText = lists:flatten(
                              begin
                                  {_, MeaningListText} =
                                      lists:foldl(
                                        fun({tag, Tag}, {_, Acc}) ->
                                                {1, Acc ++ io_lib:format("~n(~s): ", [Tag])};
                                           ({meanings, MeaningEntry}, {EntryNum, Acc}) ->
                                                {EntryNum + 1, Acc ++ io_lib:format("~w. ~s~n", [EntryNum, MeaningEntry])}
                                        end,
                                        {1, ""},
                                        Meanings),
                                  MeaningListText
                              end),
              io_lib:format("~n~s [~s] ~s", [Spelling, Reading, MeaningText])
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

collect_meanings({<<"div">>,
                  [{<<"class">>,
                   <<"meaning-wrapper">>}],
                  [{<<"div">>,
                    [{<<"class">>,
                      <<"meaning-definition zero-padding">>}],
                   Contents}]}) ->
    Meanings = lists:filter(
                 fun({<<"span">>,
                      [{<<"class">>,<<"meaning-meaning">>}],
                      _}) -> true;
                    ({<<"span">>,
                      [{<<"class">>,<<"meaning-abstract">>}],
                      _}) -> true;
                    (_) -> false
                 end,
                 Contents),
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
                                  Meaning);
                   ({<<"span">>,
                     [{<<"class">>,<<"meaning-abstract">>}],
                     Meaning}) ->
                        ["~n"] ++
                            lists:map(fun (M) ->
                                              case M of
                                                  {<<"span">>, _, _} ->
                                                      no;
                                                  {<<"a">>,
                                                   [{<<"href">>, Link}],
                                                   _} -> convert(Link);
                                                  MeaningText -> convert(MeaningText)
                                              end
                                      end,
                                      Meaning)
                end,
                Meanings)).

reading(Furi, Okuri) ->
    reading(Furi, Okuri, []).

reading([], [], Acc) ->
    lists:reverse(Acc);
reading(["~s"|T], [OH|OT], Acc) ->
    reading(T, OT, [io_lib:format("~s", [OH])|Acc]);
reading(["~s"|T], [], Acc) ->
    reading(T, [], [io_lib:format("~s", [""])|Acc]);
reading([FuriH|FuriT], Okur, Acc) ->
    reading(FuriT, Okur, [FuriH|Acc]).

get_furi({<<"span">>, [{<<"class">>,<<"furigana">>}], Contents}) ->
    %% well, shit
    %% here's the example:
    %% <span class="furigana">
    %%   <span class="kanji-3-up kanji">しっぽ</span>
    %%   <span></span>
    %% </span>
    %% <span class="text">
    %%   尻尾
    %% </span>
    %% what do we have here? blank spans in furigana section without corresponding okurigana!
    %% unacceptable (and causing errors!)
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
                        %% Good example:
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
                      %% Good example:
                      %% <span class="text">
                      %%     知<span>り</span>合<span>い</span>
                      %% </span>
                      case Text of
                          {<<"span">>, [], []} -> "";
                          {<<"span">>, [], [Okurigana]} -> convert(Okurigana);
                          Kanji -> convert(Kanji)
                      end
              end,
              Contents).

convert(BitString) ->
    string:strip(string:strip(string:strip(binary_to_list(BitString)),
                 both, $\n)).
