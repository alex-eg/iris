-module(external_interface).
-behavior(gen_server).
-export([start/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state,
	{supervisor
	}).

start(Supervisor) ->
    {ok, _Pid} = supervisor:start_child(Supervisor,
					{external_interface,
					 {?MODULE, start_link, Supervisor]},
					transient,
					5000,
					worker,
					[?MODULE]}).
start_link(Supervisor) ->
    State = #state.supervisor = Supervisor,
    gen_server:start_link({local, external_interface}, ?MODULE, State, []).

init(State) ->
    
    {ok, State}.

handle_call(Any, State) ->
    ulog:info("Recieved UNKNOWN request: ~p", [Any]),
    {noreply, State}.

handle_cast(Any, State) ->
    ulog:info("Recieved UNKNOWN cast: '~p'", [Any]),
    {noreply, State}.

handle_info(Msg, State) -> 
    ulog:info("Recieved UNKNOWN message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    ulog:info("External interface server terminated. Reason: ~p", [Reason]),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
