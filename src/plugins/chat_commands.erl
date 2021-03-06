-module(chat_commands).
-behaviour(iris_plugin).

-export([start/3, process_message/2, stop/1]).

start(_Supervisor, WorkerConfig, From) ->
    ChatCommandNames = config:get(commands, WorkerConfig),
    Rooms = config:get(rooms, WorkerConfig),
    lists:foreach(fun(RoomConfig) ->
                          RoomJid = config:get(jid, RoomConfig),
                          RoomCommandNames = config:get(commands, RoomConfig),
                          RoomCommands = lists:filtermap(fun check_module/1,
                                                         RoomCommandNames),
                          lager:info("~s command list:", [RoomJid]),
                          lists:foreach(fun(C) -> lager:info("-- ~p", [C]) end,
                                        RoomCommands),
                          jid_worker:store_config(From, RoomJid, {commands, RoomCommands})
                  end,
                  Rooms),
    ChatCommands =
        lists:filtermap(
          fun check_module/1,
          ChatCommandNames),

    lager:info("Command list:"),
    lists:foreach(fun(C) ->
                          lager:info("-- ~p", [C])
                  end,
                  ChatCommands),
    jid_worker:store_config(From, {commands, ChatCommands}).

check_module(M) ->
%% Checking command module
     ModuleOk =
        module_exists(M) andalso
        module_exports_run(M) andalso
        module_has_alias(M),
    if ModuleOk ->
            Alias = proplists:get_value(alias, M:module_info(attributes)),
            {true, {Alias, M}};
       not ModuleOk ->
            false
    end.

process_message(Message, Config) ->
    Type = message:type(Message),
    case Type of
        chat ->
            process_chat(Message, Config);
        groupchat ->
            case message:is_history(Message) of
                false -> process_groupchat(Message, Config);
                true -> ok
            end;
        error ->
            lager:warning("got XMPP error stanza: ~p", [message:raw(Message)]);
        _Other ->
            lager:error("got unknown message type: ~s", [Type])
    end.

process_chat(Message, Config) ->
    Commands = config:get(commands, Config),
    [Command|ArgList] = string:tokens(message:body(Message), " "),
    lager:debug("Chat. Command: ~p, Module: ~p", [Command, config:get(Command, Commands)]),
    case proplists:get_value(Command, Commands) of
        undefined ->
            ok;
        Module ->
            Response = Module:run(ArgList, message:from(Message)),
            lager:debug("Command ~s returned ~p", [Command, Response]),
            Jid = exmpp_xml:get_attribute(message:raw(Message), <<"from">>, undefined),
            jid_worker:reply(Response, Jid)
    end.

process_groupchat(Message, _Config) ->
    FromRoom = message:from_room(Message),
    Commands = jid_worker:get_config(self(), FromRoom, commands),
    case string:tokens(message:body(Message), " ") of
        [MaybeCommand|ArgList] -> ok;
        [] ->
            MaybeCommand = [],
            ArgList = []
    end,
    MaybeCommandTuple = lists:keyfind(MaybeCommand, 1, Commands),
    if MaybeCommandTuple /= false ->
            {_Alias, Command} = MaybeCommandTuple,
            Response = Command:run(ArgList,
                                   message:from(Message)),
            case Response of
                nope -> ok;
                _ -> Nick = message:nick(Message),
                     NewMessage = Nick ++ ", " ++ Response,
                     jid_worker:reply(NewMessage, FromRoom)
            end;
       MaybeCommandTuple == false ->
            ok
    end.

module_exists(Module) when is_atom(Module) ->
    try Module:module_info() of
        _InfoList ->
            true
    catch
        _:_ ->
            lager:error("Module ~p does not exist", [Module]),
            false
    end.

module_exports_run(Module) ->
    ExportsRun = lists:member({run, 2}, Module:module_info(exports)),
    if ExportsRun ->
            true;
       not ExportsRun ->
            lager:error("Module ~p does not export run/2 function", [Module]),
            false
    end.

module_has_alias(Module) ->
    HasAlias = lists:keyfind(alias, 1, Module:module_info(attributes)),
    if HasAlias == false ->
            lager:error("Module ~p does not specify alias attribute", [Module]),
            false;
       true ->
            true
    end.

stop(_From) ->
    ok.
