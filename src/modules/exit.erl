-module(exit).
-export([respond_to_message/2]).

respond_to_message(_Argument, _LastMessages) ->
    exit("I SAID SO!").
   
