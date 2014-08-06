-module(url_title).
-behaviour(iris_plugin).

-export([start/3, process_message/2]).

start(_Parent, _Config, _From) ->
    okay.

process_message(Message, Config) ->
    Type = message:type(Message),
    case Type of 
        groupchat ->
            preprocess_groupchat(Message, Config);
            %ulog:warning(?MODULE, "Message: ~p", [message:raw(Message)]);
        error ->
            ulog:warning(?MODULE, "got XMPP error stanza: ~p", [message:raw(Message)]);
        _Other ->
            ulog:error(?MODULE, "got unknown message type: ~s", [Type])
    end.

preprocess_groupchat(Message, Config) ->
    Stamp = exmpp_xml:get_element(message:raw(Message), delay), %% removing history messages
    case Stamp of
        undefined -> process_groupchat(Message, Config);
        _ -> ok
    end.

process_groupchat(Message, Config) ->
    Words = string:tokens(message:body(Message), " "),
    lists:map(fun (Word) -> process_word(Word, Message, Config) end, Words).

process_word(Word, Message, Config) ->
    Match = re:run(Word, "https?:\/\/.*"),
    case Match of
        nomatch    -> ok;
        {match, _} ->
            ulog:debug("[~s] matched ~s", [?MODULE, Word]),
            MaybeResponse = misc:httpc_request(get, {Word, []}, [], []),
            process_response(Message, MaybeResponse)
    end.

process_response(Message, {{_, 200, _}, _, Page}) ->
    MaybeTitle = extract_title(mochiweb_html:parse(Page)),
    case MaybeTitle of
        false ->
            ok;
        {<<"title">>, [], [Title]} ->
            From = exmpp_xml:get_attribute(message:raw(Message), <<"from">>, undefined),
            [RoomJid|_] = string:tokens(misc:format_str("~s",[From]),"/"),
            jid_worker:reply("Page title: " ++ Title, RoomJid);
        Any -> 
            ulog:error("[~s] got title tag: ~s", [?MODULE, Any]),
            ok
    end;
process_response(_Message, _Other) ->
    ok.

extract_title({<<"html">>, _, [Head|_]}) ->
    {<<"head">>, _, HeadChildren} = Head,
    lists:keyfind(<<"title">>, 1, HeadChildren);
extract_title({<<"head">>, _, HeadChildren}) ->
    lists:keyfind(<<"title">>, 1, HeadChildren);
extract_title(SomethingElse) ->
    "Mysterios occurence. Investigation required!",
    ulog:error("[~s] mochiveb parsed: ~t", [?MODULE, SomethingElse]).
