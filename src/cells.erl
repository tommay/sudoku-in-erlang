-module(cells).
-export([new/0, update/3, min_by_possible_size/1, do_exclusions/3,
	 to_string/1]).

%% Cells holds the state of each Cell, as a tuple containing
%% one Cell for each position on the board, numbered 0 through 80.
%% This allow us to (somewhat) abstract out the data type Puzzle uses
%% to store Cells.

%% Return a new Cells tuple with fresh Cells.

new() ->
    Cells = [cell:new(Number) || Number <- lists:seq(0, 80)],
    list_to_tuple(Cells).

%% Return a new Cells with Func applied to the Indexth element.

update(This, Index, Func) when is_number(Index), is_function(Func) ->
    spud:tuple_update(Index, Func, This).

%% Return the unplaced Cell with the smallest set of possibilities.
%% This is used to find the best Cell to make a guess for to minimize
%% the amount of guessing.

min_by_possible_size(This) ->
    spud:tuple_min_by(
      This,
      fun (Cell) ->
	      %% Sort placed Cells to the end.
	      case cell:get_possible(Cell) of
		  undefined ->
		      10;
		  Possible ->
		      possible:size(Possible)
	      end
      end).

%% Update the numbered Cells in ExclusionList to remove Digit from
%% their Possible lists.

do_exclusions(This, Digit, ExclusionList) ->
    lists:foldl(
      fun (Number, CellsAccum) ->
	      cells:update(
		CellsAccum,
		Number,
		fun (Cell) ->
			cell:not_possible(Cell, Digit)
		end)
      end,
      This,
      ExclusionList).

to_string(This) ->
    lists:map(
      fun (Cell) ->
	      case cell:get_placed(Cell) of
		  undefined ->
		      $-;
		  Digit ->
		      Digit + $0
	      end
      end,
      tuple_to_list(This)).
