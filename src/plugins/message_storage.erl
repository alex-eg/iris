-module(message_storage).
-behaviour(iris_plugin).
-behaviour(gen_server).

-export([start/3, process_message/2]).
-export([start_link/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_message/2]).

-record(state,
        {parent_worker,
         message_limit,
         ets
        }).

start(Supervisor, WorkerConfig, From) ->
    plugin_supervisor:start_plugin_process(Supervisor, ?MODULE, WorkerConfig, From).

start_link(WorkerConfig, From) ->
    Config = config:get(message_storage, WorkerConfig),
    MessageLimit = config:get(message_limit, Config),
    {ok, Pid} = gen_server:start_link(?MODULE,
                                      #state{parent_worker = From,
                                             message_limit = MessageLimit},
                                      []),
    jid_worker:store_config(From, {message_storage_server, Pid}),
    {ok, Pid}.

init(State) ->
    {ok, State#state{ets = ets:new(messages, [set])}}.

handle_call({get_message, MessageFrom, Num}, _From, State) ->
    Ets = State#state.ets,
    case config:get(MessageFrom, Ets) of
        undefined ->
            lager:warning("Queue is void for ~p", [MessageFrom]),
            Reply = "undef";
        Queue ->
            {Reply, _} = lists:foldl(fun(_, {_, Q}) ->
                                             {{value, Item}, Q2} = queue:out_r(Q),
                                             {Item, Q2}
                                     end,
                                     {"undef", Queue},
                                     lists:seq(1, Num))
    end,
    {reply, Reply, State};
handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast({store_message, From, Message}, State) ->
    Ets = State#state.ets,
    case config:get(From, Ets) of
        undefined ->
            Queue = lists:foldl(fun(_, Q) -> queue:in("undef", Q) end,
                                queue:new(),
                                lists:seq(1, State#state.message_limit));
        Queue ->
            Queue
    end,
    {{value, _Item}, NewQueue} = queue:out(Queue),
    ets:insert(Ets, {From, queue:in(Message, NewQueue)}),
    {noreply, State};
handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

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
            lager:error("got unknown message type: ~p", [Type])
    end.

process_chat(_Message, _Config) ->
    ok.

process_groupchat(Message, _Config) ->
    From = message:from(Message),
    Body = message:body(Message),
    StorageServer = jid_worker:get_config(self(), message_storage_server),
    gen_server:cast(StorageServer, {store_message, From, Body}),
    ok.

get_message(From, Num) ->
    StorageServer = jid_worker:get_config(self(), message_storage_server),
    gen_server:call(StorageServer, {get_message, From, Num}).
