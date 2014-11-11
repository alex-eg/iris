-module(core).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

-record(state,
        {supervisor,
         config_ets,
         worker_ets
        }).

start_link(SupRef) ->
    State = #state{supervisor = SupRef},
    gen_server:start_link({local, core}, ?MODULE, State, []).

init(State) ->
    lager:info("Core node started and has pid ~p", [self()]),

    %% Global config table, everyone can retrieve information from here
    %% calling core server with get_info request
    ConfigList = application:get_all_env(iris),
    ConfigEts = ets:new(config, [set]),
    lists:foreach(fun(E) ->
                          ets:insert(ConfigEts, E)
                  end,
                  ConfigList),
    WorkerEts = ets:new(workers, [set]),
    gen_server:cast(self(), start_workers),
    {ok, State#state{config_ets = ConfigEts,
                     worker_ets = WorkerEts}}.

handle_call({get_config, Key}, _From, State) ->
    Reply = config:get(Key, State#state.config_ets),
    {reply, Reply, State};
handle_call(Any, _Caller, State) -> 
    lager:info("Recieved unknown request: ~p", [Any]),
    {noreply, State}.

handle_cast({connected, From, Name}, State) ->
    lager:info("Worker ~p has connected with pid ~p, starting plugins", [Name, From]),
    ets:insert(State#state.worker_ets, {From, Name}),
    gen_server:cast(From, start_plugins),
    {noreply, State};
handle_cast({started_plugins, From, Name}, State) ->
    lager:info("Worker ~p has connected plugins, joining rooms", [Name]),
    gen_server:cast(From, join_rooms),
    {noreply, State};
handle_cast({terminated, From, Reason}, State) ->
    WorkerEts = State#state.worker_ets,
    [{_, Name}] = ets:lookup(WorkerEts, From),
    lager:info("Worker ~p for jid ~p terminated.~nReason: ~p", [Name, From, Reason]),
    ets:delete_object(WorkerEts, {From, Name}),
    {noreply, State};
handle_cast(start_workers, State) ->
    lager:info("Starting children"),
    Supervisor = State#state.supervisor,
    Config = State#state.config_ets,
    [{jids, JidConfigList}] = ets:lookup(Config, jids),
    lists:foreach(
      fun({Jid, RawJidConfig}) ->
              RawRooms = proplists:get_value(rooms, RawJidConfig),
              Rooms = [[{jid, RJid} | RestConfig] || {RJid, RestConfig} <- RawRooms],
              JidConfig = [{rooms, Rooms} | [{K,V} || {K, V} <- RawJidConfig, K =/= rooms]],
              start_worker([{jid, Jid} | JidConfig], Supervisor)
      end,
      JidConfigList),
    {noreply, State};
handle_cast({store_config, {Key, Value}}, State) ->
    ets:insert(State#state.config_ets, {Key, Value}),
    {noreply, State};
handle_cast(Any, State) ->
    lager:info("Recieved unknown cast: '~p'", [Any]),
    {noreply, State}.

handle_info(_Msg, State) ->
    lager:info("Recieved unknown message: ~p~n", [_Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    SupRef = State#state.supervisor,
    WorkerEts = State#state.worker_ets,
    ets:foldl(fun(Elem, ok) ->
                      supervisor:terminate_child(SupRef, Elem),
                      supervisor:delete_child(SupRef, Elem),
                      ok
              end,
              ok,
              WorkerEts),
    ets:delete(WorkerEts),
    lager:info("core process with pid ~p terminated. Reason: ~p",
              [self(), Reason]),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% misc utils

start_worker(Config, Supervisor) ->
    Name = list_to_atom(config:get(jid, Config)),
    lager:info("Starting jid_supervisor for ~p", [Name]),
    {ok, _Pid} = supervisor:start_child(Supervisor,
                                        {Name,
                                         {jid_supervisor, start_link, [Config, Name]},
                                         transient,
                                         infinity,
                                         supervisor,
                                         [jid_supervisor]}).
