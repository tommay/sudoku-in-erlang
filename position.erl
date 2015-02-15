-module(position).
-export([new/1, is_excluded_by/2, not_possible/2, maybe_place_forced/1]).
-export([get_possible/1, get_placed/1, get_number/1]).
-export([to_string/1]).

new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    object:new(
      position,
      [{number, Number}, {row, Row}, {col, Col}, {square, Square},
       {possible, possible:new()}, placed]).

is_excluded_by(Position = {position, _}, Other = {position, _}) ->
    %% Shouldn't have to check that it's the same Position.
    get_number(Position) /= get_number(Other) andalso
	(object:get(Position, row) == object:get(Other, row) orelse
	 object:get(Position, col) == object:get(Other, col) orelse
	 object:get(Position, square) == object:get(Other, square)).

%% Returns NewPosition.
%%
not_possible(Position = {position, _}, Digit) ->
    object:update(
      Position, possible,
      fun (Possible = {possible, _}) ->
	      possible:remove(Possible, Digit)
      end).

%% Returns {ok, NewPosition} if successful.
%%
maybe_place_forced(Position = {position, _}) ->
    Possible = object:get(Position, possible),
    object:maybe_update(
      Position, placed,
      fun (Placed) ->
	      case Placed == undefined andalso possible:size(Possible) == 1 of
		  true ->
		      Forced = possible:first(Possible),
		      %% XXX Set possible to set with just Forced if update
		      %% says we updated?
		      {ok, Forced};
		  false ->
		      not_updated
	      end
      end).

get_number(Position = {position, _}) ->
    object:get(Position, number).

get_placed(Position = {position, _}) ->
    object:get(Position, placed).

get_possible(Position = {position, _}) ->
    object:get(Position, possible).

to_string(Position = {position, _}) ->
    spud:format("~w ~w ~s", [get_number(Position), get_placed(Position),
			     possible:to_string(get_possible(Position))]).
