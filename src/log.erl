-module(log).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("xmpp.hrl").

-record(state, 
        {log_dir,
         name
        }).

start_link(LogDir, StringName) ->
    Name = list_to_atom(StringName),
    State = #state{
               log_dir = LogDir,
               name = Name
              },
    ok = check_directory(LogDir),
    gen_server:start_link({local, Name}, ?MODULE, State, []).
   
init(State) ->
    {ok, State}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast({store_message, Message}, State) ->
    store_message(Message),
    {noreply, State};
handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Any, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ulog:info("Log server ~s stopped for the following reason: ~s",
               [State#state.name, Reason]),
    ok.

code_change(_OldVersion, State, _Extra) ->
    ulog:debug("Info log's code changed!"),
    {ok, State}.


check_directory(LogDir) ->
    DirStatus = file:make_dir(LogDir),
    case DirStatus of
        {error, eacces} ->
            ulog:error("Missing search or write permissions for the parent directories of ~s.", [LogDir]),
            error;
        {error, eexist} ->
            ok;
        {error, enoent} ->
            ulog:error("A component of ~s does not exist.", [LogDir]),
            error;
        {error, enospc} ->
            ulog:error("No space left on the device."),
            error;
        {error, enotdir} ->
            ulog:error("A component of ~s is not a directory.", [LogDir]),
            error;
        ok ->
            ok
    end.
        
store_message(Message) ->
    ok.
