-module(word2).
%-export([run/2]).
-behaviour(iris_command).
-alias("@word2").

-compile(export_all).

run([], _) ->
    "What is the sound of one hand clapping?";
run(Args, _) ->
    [{request_url, Base}] = jid_worker:get_config(denshi_jisho),
    [Head|Tail] = Args,
    QueryURL = make_request_url(Head, Tail, Base) ++ "%23words", % for #words, to tell jisho we are searching words
    {{_, 200, _}, _, Response} = misc:httpc_request(get, {QueryURL, []}, [], []),
    Dom = mochiweb_html:parse(Response),
    extract_info(Dom).

make_request_url("en", Tail, Base) ->
    Query = create_query(Tail),
    io_lib:format(Base, ["", Query]);
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
    %% F = fun(List) -> {X, _} = lists:split(MaxLen, List),
    %%                  X
    %%     end,
    %% lists:flatten(io_lib:format("~n~p", [lists:nth(1, lists:zip3(F(Furi), F(Text), F(MeaningTags)))])).
    ReadingTemplate = lists:foldl(
                        fun (S, A) ->
                                A ++ io_lib:format("~s", [S])
                        end,
                        "",
                        word2:get_furi(lists:nth(2, Furi))),
    Okuri = get_okuri(lists:nth(2, Text)),

    %%io_lib:format("~s [~s] ~s", [Spelling, Reading, Meanings]).
    %%Furi.
    io_lib:format(io_lib:format(ReadingTemplate), Okuri).

get_furi({<<"span">>, [{<<"class">>,<<"furigana">>}], Contents}) ->
    lists:map(fun(C) ->
                      {<<"span">>, _, Text} = C,
                      case Text of
                          [] -> "~s";
                          [Kana] -> binary_to_list(Kana)
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
                            {<<"span">>, _, [Okuri]} -> [binary_to_list(Okuri)|A];
                            _ -> A
                        end
		end,
		[],
		Contents).
    

get_kanji({<<"span">>, [{<<"class">>,<<"text">>}], Contents}) ->
    lists:map(fun(C) ->
                      {<<"span">>, _, Text} = C,
                      %% Good exapmle:
                      %% <span class="text">
                      %%     知<span>り</span>合<span>い</span>
                      %% </span>
                      case Text of
                          [] -> "";
                          [Kanji] -> binary_to_list(Kanji);
                          {<<"span">>, [], [Okurigana]} -> binary_to_list(Okurigana)
                      end
              end,
              Contents).
