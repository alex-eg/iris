-module(dummy_server).
-behavior(gen_server).
-behavior(iris_plugin).
-export([start/2, process_message/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
        {parent_worker}).

start(_WorkerConfig, From) ->
    State = #state{parent_worker = From},
    {ok, _Pid} = gen_server:start_link(?MODULE, State, []).

init(State) ->
    {ok, State}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:info("Dummy server terminated. Reason: ~p", [Reason]),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% gen_server callbacks end

process_message(_Msg, _Cfg) ->
    nope.

