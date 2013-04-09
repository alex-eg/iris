-module(root).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

start_link(SupRef) ->
    gen_server:start_link({local, root}, ?MODULE, SupRef, []).

init(ParentPid) ->
    application:start(exmpp),
    ulog:info("Root node started and has PID ~p, parent process is ~p", [self(), ParentPid]),
    ConfigList = config:init("priv/cfg.erl"),
    
    %% Todo: issue #13
    ets:new(config, [named_table, bag]),
    
    ets:new(workers, [named_table, bag]),
    self() ! start_children,
    {ok, {ParentPid, ConfigList}}.

handle_cast({connected, From, Name}, State) ->
    ulog:info("Worker ~p has connected with pid ~p, now entering rooms...~n", [Name, From]),
    ets:insert(workers, {From, Name}),
    gen_server:cast(From, join_rooms),
    {noreply, State};
handle_cast({terminated, From, Reason}, State) ->
    [{_, Name}] = ets:lookup(workers, From),
    ulog:info("Worker ~p for jid ~p terminated.~nReason: ~p", [Name, From, Reason]),
    ets:delete_object(workers, {From, Name}),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved UNKNOWN cast: '~p'", [Any]),
    {noreply, State}.

handle_info(start_children, State) ->
    ulog:info("Starting children"),
    {Supervisor, ConfigList} = State,
    JidConfigs = proplists:get_all_values(jid_config, ConfigList),
    lists:foreach(fun(ConfigEntry) ->
			  ConfigRecord = config:parse(jid_config, 
						      ConfigEntry),
			  start_worker(ConfigRecord, Supervisor)
		  end,
		  JidConfigs),
    {noreply, State};
handle_info(_Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: ~p~n", [_Msg]),
    {noreply, State}.

%% Todo: issue #13
handle_call({get_config}, _From, State) ->
    
handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
   
%% gen_server callbacks end

start_worker(Config, SupRef) ->
    Name = list_to_atom(Config#jid_info.jid),
    ulog:info("Starting worker ~p with supervisor ~p", 
	      [Name,
	       SupRef
	      ]),
    {ok, _Pid} = supervisor:start_child(SupRef,
				       {Name,
					{jid_worker, start_link, [Config, Name]},
					transient,
					5000,
					worker,
					[jid_worker]}).
