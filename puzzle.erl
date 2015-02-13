-module(puzzle).
-export([new/1,solve/1]).

new(Setup) ->
    object:new(
      puzzle,
      [{positions, positions:new(Setup)}].

solve(Puzzle = {puzzle, _}) ->
    spud:do_while(Puzzle, fun maybe_place_or_eliminate/1).

maybe_place_or_eliminate(Puzzle = {puzzle, _}) ->
    spud:or_else(Puzzle,
		 [fun maybe_place_one_missing/1,
		  fun maybe_place_one_forced/1,
		  fun maybe_eliminate_with_tricky_sets/1]).
   
maybe_place_one_missing(_Puzzle = {puzzle, nyi}) ->
    false.

# Returns {ok, NewPuzzle} or {not_updated, Puzzle}.
#
maybe_place_one_forced(Puzzle = {puzzle, _}) ->
    object:maybe_update(
      Puzzle, positions,
      fun positions:maybe_update_one_forced/1).

maybe_eliminate_with_tricky_sets(_Puzzle = {puzzle, nyi}) ->
    false.

create_positions(Setup) ->
    Digits = lists:map(
	       fun (Char) ->
		       case Char of
			   45 ->
			       undefined;
			   _ ->
			       Char - 48
		       end
	       end,
	       Setup),
    Seq = lists:seq(0, 80),
    Zipped = lists:zip(Seq, Setup),
    lists:map(
      fun ({N, Digit}) ->
	      position:new(N, Digit)
      end,
      Zipped).

