-module(position).
-export([new/1, is_excluded_by/2, place_forced/1]).

new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    object:new(
      position,
      [{number, Number}, placed, {row, Row}, {col, Col}, {square, Square}]).

is_excluded_by(Position = {position, _}, Other = {position, _}) ->
    object:get(Position, number) /= object:get(Other, number) and
	(object:get(Position, row) == object:get(Other, row) or
	 object:get(Position, col) == object:get(Other, col) or
	 object:get(Position, square) == object:get(Other, square)).

place_forced(Position = {position, _}) ->
    Possible = possible(Position),
    case placed(Position) == undefined and spud:length(Possible) == 1 of
	true ->
	    io.format("placing forced #{position.possible.first} in position #{position.number}"),
	    print_puzzle,
	    {ok, place(position, Position, hd(Possible))};
	false ->
	    {false, Position}
    end.

possible(Position = {position, _}) ->
    object:get(Position, possible).

place(Position = {position, _}, Digit) ->
    set(Position, Digit).
