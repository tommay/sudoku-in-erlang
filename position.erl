-module(position).
-export([new/1, is_excluded_by/2, not_possible/2, maybe_place_forced/1]).

new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    object:new(
      position,
      [{number, Number}, {row, Row}, {col, Col}, {square, Square},
       {possible, create_possible()}, place]).

is_excluded_by(Position = {position, _}, Other = {position, _}) ->
    object:get(Position, number) /= object:get(Other, number) and
	(object:get(Position, row) == object:get(Other, row) or
	 object:get(Position, col) == object:get(Other, col) or
	 object:get(Position, square) == object:get(Other, square)).

% Returns NewPosition.
%
not_possible(Position = {position, _}, Digit) ->
    object:update(
      Position, possible,
      fun (Possible) ->
	      sets:del_element(Possible, Digit)
      end).

% Returns {ok, NewPosition} is successful.
%
maybe_place_forced(Position = {position, _}) ->
    Possible = object:get(Position, possible),
    object:maybe_update(
      Position, placed,
      fun (Placed) ->
	      case Placed == undefined and sets:size(Possible) == 1 of
		  true ->
		      io.format("placing forced #{position.possible.first} in position #{position.number}"),
		      print_puzzle,
		      Forced = hd(sets:to_list(Possible)),
		      % XXX Set possible to set with just Forced if update
		      % says we updated?
		      {ok, Forced};
		  false ->
		      not_updated
	      end
      end).
