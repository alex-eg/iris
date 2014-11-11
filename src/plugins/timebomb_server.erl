-module(timebomb_server).
-behavior(gen_server).
-behavior(iris_plugin).
-export([start/3, start_link/2, process_message/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start(Supervisor, _WorkerConfig, From) ->
    plugin_supervisor:start_plugin_process(Supervisor, ?MODULE, [], From).

start_link(_WorkerConfig, _From) ->
    Timeout = config:get(timeout, jid_worker:get_config(timebomb_server)),
    {ok, _Pid} = gen_server:start_link(?MODULE, Timeout, []).

init(Timeout) ->
    lager:debug("Timeout is ~p", [Timeout]),
    timer:send_after(Timeout, exit),
    {ok, []}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(exit, _State) ->
    exit("Killed by timebomb server");
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(_Msg, _Cfg) ->
    nope.

