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

place_one_forced(Type = puzzle, Puzzle = {Type, _}) ->
    object:update(puzzle, Puzzle, positions,
		  fun spud:update_one(position,
				      position:place_forced/2).

eliminate_with_tricky_sets(Type = puzzle, _Puzzle = {Type, nyi}) ->
    false.

