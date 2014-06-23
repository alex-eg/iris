-module(external_interface).
-export([init/1, process_message/2]).

init(_Config) ->
    wut.

process_message(_Message, _Config) ->
    pass.
