-module(positions).
-export([new/0, update/3, min_by_possible_size/1, to_string/1]).

%% This allow us to (somewhat) abstract out the data type Puzzle uses
%% to store Positions.

new() ->
    List = [position:new(Number) || Number <- lists:seq(0, 80)],
    array:from_list(List).

update(This, Index, Func) when is_number(Index), is_function(Func) ->
    spud:array_update(Index, Func, This).

%% Returns the unplaced Position with the smallest set of
%% possibilities.  This is used to find the best Position to make a
%% guess for to minimize the amount of guessing.
%%
min_by_possible_size(This) ->
    spud:array_min_by(
      This,
      fun (Position) ->
	      %% Sort placed positions to the end.
	      case position:get_placed(Position) of
		  undefined ->
		      Possible = position:get_possible(Position),
		      possible:size(Possible);
		  _ ->
		      10
	      end
      end).

to_string(This) ->
    lists:map(
      fun (Position) ->
	      case position:get_placed(Position) of
		  undefined ->
		      $-;
		  Digit ->
		      Digit + $0
	      end
      end,
      array:to_list(This)).
