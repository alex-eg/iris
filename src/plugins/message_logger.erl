-module(message_logger).
-behaviour(iris_plugin).

-export([start/3, process_message/2, stop/1]).

-define(HEADER,
        "<!DOCTYPE html>~n"
        "<html>~n"
        "<head>~n"
        "<meta charset='UTF-8'>"
        "<title>Log of ~s with ~s on ~p-~p-~p</title>~n" % bot jid, chat name, date
        "</head>~n"
        "<body>~n"
       ).

-define(FORMAT_CHAT,
        "<div class='chat-message'>~n"
        "<span class='time'>~s</span>"
        "<span class='chat-nick'>~s</span>"
        "<span class='message'>~s</span>~n"
        "</div>~n"
       ).

-define(FORMAT_GROUPCHAT,
        "<div class='groupchat-message'>~n"
        "<span class='time'>~s</span>"
        "<span class='groupchat-nick'>~s</span>"
        "<span class='message'>~s</span>~n"
        "</div>~n"
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
    OpenedFiles =  jid_worker:get_config(self(), message_logger_opened_files),
    File = get_file(From, OpenedFiles, Config),

    Time = current_time(),
    Body = message:body(Message),
    Entry = lists:flatten(io_lib:format(?FORMAT_CHAT, [Time, From, Body])),

    ok = file:write(File, list_to_binary(Entry)).

save_groupchat_message(Message, Config) ->
    Room = message:from_room(Message),
    OpenedFiles = jid_worker:get_config(self(), message_logger_opened_files),
    File = get_file(Room, OpenedFiles, Config),

    Time = current_time(),
    Nick = message:nick(Message),
    Body = message:body(Message),
    Entry = lists:flatten(io_lib:format(?FORMAT_GROUPCHAT, [Time, Nick, Body])),

    ok = file:write(File, list_to_binary(Entry)).

current_time() ->
    {_, {H, Min, S}} = calendar:now_to_datetime(erlang:timestamp()),
    lists:flatten(io_lib:format("~p:~p:~p", [H, Min, S])).

get_file(From, OpenedFiles, Config) ->
    %% If file is not already opened - open file and insert it
    %% into jid_worker ets
    case proplists:lookup(From, OpenedFiles) of
        none ->
            BaseDir = jid_worker:get_config(self(), message_logger_dir),
            ChatDir = filename:join(BaseDir, "./" ++ From),
            filelib:ensure_dir(ChatDir),
            {{Y, M, D}, _} = calendar:now_to_datetime(erlang:timestamp()),
            Filename = lists:flatten(io_lib:format("~p-~p-~p.html", [Y, M, D])),
            PathToFile = filename:join(ChatDir, "./" ++ Filename),

            lager:debug("Opening ~s for writing", [PathToFile]),

            case file:read_file_info(PathToFile) of
                {error, enoent} ->
                    %% File is not created, creating
                    filelib:ensure_dir(PathToFile),
                    {ok, File} = file:open(PathToFile, [write]),
                    Jid = jid_worker:get_config(self(), jid),
                    Header = lists:flatten(io_lib:format(?HEADER, [Jid, From, D, M, Y])),
                    file:write(File, list_to_binary(Header));
                _ ->
                    %% File already present
                    {ok, File} = file:open(PathToFile, [append]),
                    locate_footer(File)
            end,
            ets:insert(Config, {message_logger_opened_files, [{From, File}|OpenedFiles]}),
            File;
        {_, File} -> File
    end.

save_other_message(_Message, _Config) ->
    ok.

stop(From) ->
    OpenedFiles = jid_worker:get_config(From, message_logger_opened_files),
    lists:foreach(fun({From, F}) ->
                          lager:debug("Closing file for ~p", [From]),
                          file:write(F, lists:flatten(io_lib:format(?FOOTER, []))),
                          file:sync(F),
                          file:close(F)
                  end,
                  OpenedFiles).

locate_footer(File) ->
    %% Set location to two lines before end of file
    %% since we don't want to append new messages after
    %% </body> and </html> tags
    {ok, _} = file:position(File, {eof, erlang:byte_size(unicode:characters_to_binary(?FOOTER))}).
