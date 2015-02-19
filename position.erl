-module(position).
-export([new/1, get_number/1, get_possible/1, place/2, get_placed/1]).
-export([is_excluded_by/2, not_possible/2]).
-export([to_string/1]).

%% Returns a new Position at position Number.  Number is a number from
%% 0 to 80 denoting where this Position is on the grid.  Figure out
%% and remember which row, column, and square this Position is in.  A
%% Position has set of digits it can possible contain without
%% conflicting with other previously placed positions.  "placed"can be
%% either undefined, or the digit that has been placed in this position.
%%
new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    object:new(
      position,
      [{number, Number}, {row, Row}, {col, Col}, {square, Square},
       {possible, possible:new()}, placed]).

get_number(This = {position, _}) ->
    object:get(This, number).

get_possible(This = {position, _}) ->
    object:get(This, possible).

place(This = {position, _}, Digit) ->
    object:set(This, [{placed, Digit}, {possible, undefined}]).

get_placed(This = {position, _}) ->
    object:get(This, placed).

%% Returns true if This and Other are in the same row, column, or
%% square, else false.
%%
is_excluded_by(This = {position, _}, Other = {position, _}) ->
    %% Shouldn't have to check that it's the same Position.
    get_number(This) /= get_number(Other) andalso
	(object:get(This, row) == object:get(Other, row) orelse
	 object:get(This, col) == object:get(Other, col) orelse
	 object:get(This, square) == object:get(Other, square)).

%% Returns NewPosition with Digit remove from the possible set.
%%
not_possible(This = {position, _}, Digit) ->
    object:update(
      This, possible,
      fun (Possible = {possible, _}) ->
	      possible:remove(Possible, Digit);
	  (undefined) ->
	      undefined
      end).

to_string(This = {position, _}) ->
    spud:format("~w ~w ~s", [get_number(This), get_placed(This),
			     possible:to_string(get_possible(This))]).
