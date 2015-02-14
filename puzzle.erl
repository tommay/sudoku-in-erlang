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

-solve(Positions = {positions, _}) ->
    % We get here either because we're done, we've failed, or we have
    % to guess and recurse.  We can distinguish by examining the
    % position with the fewest possibilities remaining.

    NextPosition = min_by_possible(Positions),

    case object:get(NextPosition, placed) /= undefined of
	true ->
            % Solved.  Return Positions as a solution.
	    % puts "Solved:"
	    % print_puzzle
	    {solved, Positions};
	false ->
	    Possible = object.get(NextPosition, possibile),
	    case sets.size(Possible)) of
		0 ->
		    % Failed.  No solution to return.
		    % puts "Backing out."
		    {failed};
		1 ->
		    % Found an unplaced position with only one possibility.
		    % Place it and iterate by calling solve recursively.
		    % puts "placing forced #{next_position.possible.first} in position #{next_position.number}"
		    % print_puzzle
		    solve(positions:place(NextPosition, Possible.first));
		_ ->
		    % Found an unplaced position with multiple
		    % possibilities.  Guess each possibility
		    % recursively, and return any solutions we find.
		    Possible.each do |digit|
        puts "trying #{digit} in position #{next_position.number} #{next_position.possible}"
        @@stats[:guesses] += 1
        puzzle = Puzzle.new(setup: to_string)
        puzzle.position(next_position.number).place(digit)
        puzzle.solve(yielder)
      end
    end
