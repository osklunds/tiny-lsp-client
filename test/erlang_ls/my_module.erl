-module(my_module).

-export([my_function/1]).

other_function(Arg) ->
    io:format("~p~n", [Arg]).



my_function(Arg) ->
    other_function({arg, Arg}).
