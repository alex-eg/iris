-module(message_logger).
-behaviour(iris_plugin).

-export([start/3, process_message/2, stop/1]).

-define(HEADER,
        "<!DOCTYPE html>~n"
        "<html>~n"
        "<head>~n"
        "<title>Log of ~s with ~s on ~p-~p-~p</title>~n" % bot jid, chat name, date
        "</head>~n"
        "<body>~n"
       ).

-define(FORMAT_CHAT,
        "<div class='time'>~s</div>"
        "<div class='chat-nick'>~s</div>"
        "<div class='message'>~s</div>~n"
       ).

-define(FORMAT_GROUPCHAT,
        "<div class='time'>~s</div>"
        "<div class='groupchat-nick'>~s</div>"
        "<div class='message'>~s</div>~n"
        ).

-define(FOOTER,
        "</body>~n"
        "</html>~n"
       ).

start(_Supervisor, _WorkerConfig, From) ->

    Config = jid_worker:get_config(message_logger),
    AllLogDir = filename:absname(config:get(dir, Config)),
    InstanceLogDir = filename:join(AllLogDir, jid_worker:get_config(self(), jid)),
    jid_worker:store_config(From, {message_logger_dir, InstanceLogDir}),
    jid_worker:store_config(From, {message_logger_opened_files, []}).

process_message(Message, Config) ->
    Type = message:type(Message),
    case Type of
        chat ->
            save_chat_message(Message, Config);
        groupchat ->
            save_groupchat_message(Message, Config);
        _Other ->
            save_other_message(Message, Config)
    end.

save_chat_message(Message, Config) ->
    From = message:from(Message),
    Body = message:body(Message),
    OpenedFiles =  jid_worker:get_config(self(), message_logger_opened_files),
    lager:debug("Saving message ~s from ~s", [Body, From]),
    lager:debug("Opened files: ~p", [OpenedFiles]),
    case proplists:lookup(From, OpenedFiles) of
        none ->
            BaseDir = jid_worker:get_config(self(), message_logger_dir),
            ChatDir = filename:join(BaseDir, "./" ++ From),
            filelib:ensure_dir(ChatDir),
            {{Y, M, D}, _} = calendar:now_to_datetime(erlang:timestamp()),
            Filename = lists:flatten(io_lib:format("~p-~p-~p.html", [Y, M, D])),
            PathToFile = filename:join(ChatDir, "./" ++ Filename),
            lager:debug("Opening ~s for writing", [PathToFile]),
            filelib:ensure_dir(PathToFile),

            {ok, File} = file:open(PathToFile, [append]),

            Jid = jid_worker:get_config(self(), jid),
            Header = lists:flatten(io_lib:format(?HEADER, [Jid, From, D, M, Y])),
            file:write(File, list_to_binary(Header)),
            jid_worker:store_config(self(), {message_logger_opened_files, [{From, File}|OpenedFiles]});

%            delete_footer(File);
        {_, File} -> ok
    end,

    {_, {H, Min, S}} = calendar:now_to_datetime(erlang:timestamp()),
    Body = message:body(Message),
    Time = lists:flatten(io_lib:format("~p:~p:~p", [H, Min, S])),
    Entry = lists:flatten(io_lib:format(?FORMAT_CHAT, [Time, From, Body])),

    ok = file:write(File, list_to_binary(Entry)).

save_groupchat_message(Message, Config) ->
    Room = message:from_room(Message),
    Nick = message:nick(Message),
    Body = message:body(Message),
    OpenedFiles = jid_worker:get_config(self(), message_logger_opened_files),
    case proplists:lookup(Room, OpenedFiles) of
        none ->
            BaseDir = jid_worker:get_config(self(), message_logger_dir),
            ChatDir = filename:join(BaseDir, "./" ++ Room),
            filelib:ensure_dir(ChatDir),
            {{Y, M, D}, _} = calendar:now_to_datetime(erlang:timestamp()),
            Filename = lists:flatten(io_lib:format("~p-~p-~p.html", [Y, M, D])),
            PathToFile = filename:join(ChatDir, "./" ++ Filename),
            lager:debug("Opening ~s for writing", [PathToFile]),
            filelib:ensure_dir(PathToFile),
            {ok, File} = file:open(PathToFile, [append]),

            Jid = jid_worker:get_config(self(), jid),
            Header = lists:flatten(io_lib:format(?HEADER, [Jid, Room, D, M, Y])),
            file:write(File, list_to_binary(Header)),
            delete_footer(File),
            ets:insert(Config, {message_logger_opened_files, [{Room, File}|OpenedFiles]});
        {_, File} -> ok
    end,

    {_, {H, Min, S}} = calendar:now_to_datetime(erlang:timestamp()),
    Body = message:body(Message),
    Time = lists:flatten(io_lib:format("~p:~p:~p", [H, Min, S])),
    Entry = lists:flatten(io_lib:format(?FORMAT_GROUPCHAT, [Time, Nick, Body])),

    ok = file:write(File, list_to_binary(Entry)).

save_other_message(_Message, _Config) ->
    ok.

stop(From) ->
    OpenedFiles = jid_worker:get_config(From, message_logger_opened_files),
    lists:foreach(fun({_, F}) ->
                          file:write(F, lists:flatten(io_lib:format(?FOOTER, []))),
                          file:sync(F),
                          file:close(F)
                  end,
                  OpenedFiles).

delete_footer(File) ->
    ok.
    %% case file:read_line(File) of
    %%     eof ->
    %%         ok;
    %%     {ok, _Data} ->
    %%         ok
    %% end.
