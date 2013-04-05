-module(root).
-behavior(gen_server).
-export([start/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

start(SupRef) ->
    application:start(exmpp),
    gen_server:start_link({local, root}, ?MODULE, SupRef, []).

init(ParentPid) ->
    ulog:info("Root node started and has PID ~p, parent process is ~p", [self(), ParentPid]),
    ConfigList = config:init("cfg.erl"),
    ets:new(workers, [named_table, bag]),
    self() ! start_children,
    {ok, {ParentPid, ConfigList}}.

handle_cast({connected, From}, State) ->
    ulog:info("Worker ~p has connected, now entering rooms...~n", [From]),
    gen_server:cast(From, join_rooms),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved message: '~p'", [Any]),
    {noreply, State}.

handle_info(start_children, State) ->
%%    ulog:info("Starting!! children state is ~p", [State]),
    {Supervisor, ConfigList} = State,
    %% ulog:debug("Config list is ~p", [ConfigList]),
    %% ulog:debug("Config dist is ~p", [ConfigList]),
    %% ulog:debug("Config fist is ~p", [ConfigList]),
    %% ulog:debug("Config gist is ~p", [ConfigList]),
    %% ulog:debug("Config jist is ~p", [ConfigList]),
    lists:foreach(fun(ConfigEntry) ->
			  ulog:debug("Parsing config entry"),
			  ConfigRecord = config:parse(ConfigEntry),
			  ulog:debug("Config record is ~p", [ConfigRecord]),
			  start_worker(ConfigRecord, Supervisor)
		  end,
		  ConfigList),
    {noreply, State};
 
handle_info(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
   
%% gen_server callbacks end

start_worker(Config, SupRef) ->
    Name = list_to_atom(Config#jid_info.jid),
    ulog:info("Starting worker for jid ~p with supervisor ~p", 
	      [Config#jid_info.jid,
	       SupRef
	      ]),
    {ok, Pid} = supervisor:start_child(SupRef,
				       {Name,
					{jid_worker, start, [Config, Name]},
					permanent,
					brutal_kill,
					worker,
					[jid_worker]}),
    ulog:debug("Worker started with pid ~p", [Pid]),
    ets:insert(workers, {Pid, Name}).
