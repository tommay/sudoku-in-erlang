-module(position).
-export([new/1, place_forced/2]).

new(Number) ->
    object:new(position, [{number, Number}, placed]).

place_forced(Position = {position, _}) ->
    Possible = possible(Position),
    case not placed(Position) and spud:length(Possible) == 1 of
	true ->
	    io.format("placing forced #{position.possible.first} in position #{position.number}"),
	    print_puzzle,
	    {ok, place(Position, spud:first(Possible))};
	false ->
	    {false, Position}
    end.

possible(Position = {position, _}) ->
    object:get(position, Position, possible).

place(Position, Digit) ->
    % XXX exclusion sets.
    set(position, Position, Digit).
