-module(iris).
-behavior(gen_server).
-export([start/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

start() ->
    application:start(exmpp),
    config:init("cfg.erl"),
    ConfigRecord = create_config_record(),
    gen_server:start_link({local, root}, ?MODULE, ConfigRecord, []).

init(State) ->
    ulog:info("Supervisor process started and has PID ~p", [self()]),
    ets:new(workers, [named_table, bag]),
    start_worker(State),
    {ok, State}.

handle_cast({connected, From}, State) ->
    ulog:info("Worker ~p has connected", [From]),
    {noreply, State};
handle_cast({respawn, From}, State) ->
    ulog:info("Respawning worker", []),
    start_worker(State),
    {noreply, State};
handle_cast(Any, State) ->
    ulog:info("Recieved message: '~p'", [Any]),
    {noreply, State}.
 
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
   
create_config_record() ->
    #bot_info {
       server_address = config:get(server),
       jid = config:get(jid),
       password = config:get(password),
       room = config:get(room),
       timeout = config:get(timeout),
       nick = config:get(nick),
       modules = config:get(modules)
      }.

start_worker(Config) ->
    ulog:info("Starting worker on server ~p for jid ~p with supervisor ~p", [Config#bot_info.server_address, Config#bot_info.jid, self()]),
    gen_server:start_link({local, worker}, worker, Config, []),
    enter_rooms().
    
enter_rooms() ->
    Room = config:get(room),
    RoomJid = Room ++ "@conference." ++ config:get(server),
    Nick = config:get(nick),
    gen_server:cast(worker, {join, RoomJid, Nick}).

