-module(puzzle).
-export([solve/1]).

Puzzle = object:new(puzzle, [{positions, create_positions()}]).

create_positions() ->
    lists:map(0..80, fun (N) -> position:new(N) end).

solve(Puzzle = {puzzle, _}) ->
    spud:do_while(Puzzle, fun place_or_eliminate/1).

place_or_eliminate(Puzzle = {puzzle, _}) ->
    spud:or_else(Puzzle,
		 [fun place_one_missing/1,
		  fun place_one_forced/1,
		  fun eliminate_with_tricky_sets/1]).
   
place_one_missing(_Puzzle = {puzzle, nyi}) ->
    false.

# Returns {ok, NewPuzzle} or {not_updated, Puzzle}.
#
place_one_forced(Puzzle = {puzzle, _}) ->
    Updated = object:maybe_update(
		Puzzle, positions,
		fun spud:maybe_update_one(position:maybe_place_forced/1)),
    case Updated of
	{ok, NewPuzzle, ChangedPosition} ->
	    {ok, do_exclusions(Puzzle, ChangedPosition)};
	_ ->
	    {not_updated, Puzzle}
    end.

# Returns new Puzzle.
#
do_exclusions(Puzzle = {puzzle, _}, ChangedPosition = {position, _}) ->
    Digit = object:get(ChangedPosition, placed),
    object:update(
      Puzzle, positions,
      fun (Positions) ->
	      lists:map(
		fun (Position = {position, _}) ->
			case position:is_excluded_by(Position, ChangedPosition) of
			    true ->
				position:not_possible(Position, Digit);
			    false ->
				Position
			end
		end,
		Positions)
      end).


eliminate_with_tricky_sets(_Puzzle = {puzzle, nyi}) ->
    false.

