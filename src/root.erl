-module(root).
-behavior(gen_server).
-export([start/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

start() ->
    application:start(exmpp),
    ConfigList = config:init("cfg.erl"),
    gen_server:start_link({local, root}, ?MODULE, ConfigList, []).

init(State) ->
    ulog:info("Main process started and has PID ~p", [self()]),
    ets:new(workers, [named_table, bag]),
    lists:foreach(fun(ConfigEntry) ->
			  ConfigRecord = config:parse(ConfigEntry),
			  start_worker(ConfigRecord)
		  end,
		  State),
    {ok, State}.

handle_cast({connected, From}, State) ->
    ulog:info("Worker ~p has connected, now entering rooms...~n", [From]),
    gen_server:cast(From, join_rooms),
    {noreply, State};
handle_cast({respawn, _From}, State) ->
    ulog:info("Respawning worker", []),
    start_worker(State),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved message: '~p'", [Any]),
    {noreply, State}.
 
handle_info({'EXIT', From, Reason}, State) ->
    ulog:info("Worker ~p exited. Reason: ~p. Restarting in ~pms.",
	      [From, Reason, ?RESTART_TIMEOUT]),
    timer:apply_after(?RESTART_TIMEOUT, 
		      ?MODULE,
		      gen_server:cast(),
		      [{respawn, self()}]),
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
   
start_worker(Config) ->
    ulog:info("Starting worker for jid ~p with supervisor ~p", 
	      [Config#jid_info.jid,
	       self()
	      ]),
    Name = list_to_atom(Config#jid_info.jid),
    {ok, Pid} = gen_server:start_link({local, Name}, worker, Config, []),
    ets:insert(workers, {Pid, Name}).

%restart_worker(Pid) ->
 %   ulog:info("Restarting worker for jid ~p~n
