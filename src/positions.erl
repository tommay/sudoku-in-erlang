-module(positions).
-export([new/0, update/3, min_by_possible_size/1, do_exclusions/3,
	 to_string/1]).

%% Poisitions holds the state of each Position, as a tuple containing
%% one Position for each position on the board, numbered 0 through 80.
%% This allow us to (somewhat) abstract out the data type Puzzle uses
%% to store Positions.

%% Return a new Positions tuple with fresh Positions.

new() ->
    Positions = [position:new(Number) || Number <- lists:seq(0, 80)],
    list_to_tuple(Positions).

%% Return a new Positions with Func applied to the Indexth element.

update(This, Index, Func) when is_number(Index), is_function(Func) ->
    spud:tuple_update(Index, Func, This).

%% Return the unplaced Position with the smallest set of
%% possibilities.  This is used to find the best Position to make a
%% guess for to minimize the amount of guessing.

min_by_possible_size(This) ->
    spud:tuple_min_by(
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

%% Update the numbered Positions in ExclusionList to remove Digit from
%% their Possible lists.

do_exclusions(This, Digit, ExclusionList) ->
    lists:foldl(
      fun (Number, PositionsAccum) ->
	      positions:update(
		PositionsAccum,
		Number,
		fun (Position) ->
			position:not_possible(Position, Digit)
		end)
      end,
      This,
      ExclusionList).

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
      tuple_to_list(This)).
