-module(position).
-export([new/1, excluded_by/2, place_forced/2]).

new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    object:new(
      position,
      [{number, Number}, placed, {row, Row}, {col, Col}, {square, Square}]).

excluded_by(Position = {position, _}, Other = {position, _}) ->
    object:get(position, Position, number) /= object:get(position, Other, number) and
	(object:get(position, Position, row) ==
	     object:get(position, Other, row) or
	 object:get(position, Position, col) ==
	     object:get(position, Other, col) or
	 object:get(position, Position, square) ==
	     object:get(position, Other, square)).

place_forced(Type = position, Position = {Type, _}) ->
    Possible = possible(Position),
    case placed(Position) == undefined and spud:length(Possible) == 1 of
	true ->
	    io.format("placing forced #{position.possible.first} in position #{position.number}"),
	    print_puzzle,
	    {ok, place(position, Position, hd(Possible))};
	false ->
	    {false, Position}
    end.

possible(Type = position, Position = {Type, _}) ->
    object:get(position, Position, possible).

place(Type = position, Position = {Type, _}, Digit) ->
    set(position, Position, Digit).
