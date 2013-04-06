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
    ConfigList = config:init("cfg.erl"),
    ets:new(workers, [named_table, bag]),
    self() ! start_children,
    {ok, {ParentPid, ConfigList}}.

handle_cast({connected, From}, State) ->
    [{_, Name}] = ets:lookup(workers, From),
    ulog:info("Worker ~p has connected with pid ~p, now entering rooms...~n", [Name, From]),
    gen_server:cast(From, join_rooms),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved message: '~p'", [Any]),
    {noreply, State}.

handle_info(start_children, State) ->
    ulog:info("Starting children"),
    {Supervisor, ConfigList} = State,
    lists:foreach(fun(ConfigEntry) ->
			  ConfigRecord = config:parse(ConfigEntry),
			  start_worker(ConfigRecord, Supervisor)
		  end,
		  ConfigList),
    {noreply, State};
handle_info(_Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: ~p~n", [_Msg]),
    {noreply, State}.

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
    {ok, Pid} = supervisor:start_child(SupRef,
				       {Name,
					{jid_worker, start_link, [Config, Name]},
					transient,
					5000,
					worker,
					[jid_worker]}),
    ets:insert(workers, {Pid, Name}).
