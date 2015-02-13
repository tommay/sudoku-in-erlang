-module(puzzle).
-export([solve/1]).

Puzzle = object:new(puzzle, [{positions, create_positions()}]).

create_positions() ->
    lists:map(0..80, fun (N) -> position:new(N) end).

solve(Type = puzzle, Puzzle = {Type, _}) ->
    spud:do_while(puzzle, Puzzle, fun place_or_eliminate/1).

place_or_eliminate(Type = puzzle, Puzzle = {Type, _}) ->
    spud:or_else(puzzle, Puzzle,
		 [fun place_one_missing/2,
		  fun place_one_forced/2,
		  fun eliminate_with_tricky_sets/2]).
   
place_one_missing(Type = puzzle, _Puzzle = {Type, nyi}) ->
    false.

# Returns {ok, NewPuzzle} or {not_updated, Puzzle}.
#
place_one_forced(Type = puzzle, Puzzle = {Type, _}) ->
    Updated = object:update(puzzle, Puzzle, positions,
			    fun spud:update_one(position,
						position:place_forced/2)),
    case Updated of
	{ok, [NewPuzzle, ChangedPosition]} ->
	    {ok, do_exclusions(puzzle, Puzzle, position, ChangedPosition)};
	_ ->
	    {not_updated, Puzzle}
    end.

# Returns new Puzzle.
#
do_exclusions(puzzle, Puzzle = {puzzle, _}, position, ChangedPosition = {position, _}) ->
    Digit = object:get(position, ChangedPosition, placed),
    object:update(
      puzzle, Puzzle, positions,
      fun (Positions) ->
	      lists:map(
		fun (Position = {position, _}) ->
			case position:excluded_by(Position, ChangedPosition) of
			    true ->
				position:not_possible(Position, Digit);
			    false ->
				Position
			end
		end,
		Positions)
      end).


eliminate_with_tricky_sets(Type = puzzle, _Puzzle = {Type, nyi}) ->
    false.

