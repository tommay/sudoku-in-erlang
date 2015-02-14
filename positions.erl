-module(positions).
-export([new/1, solve/1, min_by_possible/1, maybe_update_one_forced/1]).

new(Setup) ->
    List = [position:new(N) || N <- lists:seq(0, 80)],
    Positions = {positions, List},
    Digits = to_digits(Setup),
    Zipped = lists:zip(List, Digits),
    {
      positions,
      lists:foldl(
	fun ({Digit, Position a= {position, _}}, Accum) ->
		case Digit of
		    undefined ->
			Accum;
		    _ ->
			place(Accum, Position, Digit)
		end
	end,
	Positions,
	Zipped)
      }.

to_digits(Setup) ->
    lists:map(
      fun (Char) ->
	      case Char of
		  45 ->
		      undefined;
		  _ ->
		      Char - 48
	      end
      end,
      Setup).

place(_Positions = {positions, List}, AtPosition, Digit) ->
    PlacedPosition = object:set(AtPosition, placed, Digit),
    Number = position:get_number(PlacedPosition),
    NewPositions = {
      positions,
      [case position:get_numer(P) == Number of
	   true -> PlacedPosition;
	   false -> P
       end || P <- List]},
    do_exclusions(NewPositions, PlacedPosition).

% Returns {ok, NewPositions} if successful.
%
maybe_update_one_forced(Positions = {positions, _}) ->
    maybe_update_one(Positions, fun position:maybe_place_forced/1).

% Returns {ok, NewPositions} if successful.
%
maybe_update_one(Positions = {positions, _}, MaybeUpdateFunc) ->
    case spud:maybe_update_one(Positions, MaybeUpdateFunc) of
	{ok, NewPositions, ChangedPosition} ->
	    {ok, do_exclusions(NewPositions, ChangedPosition)};
	Result = _ ->
	    Result
    end.

% Returns NewPositions.
%
do_exclusions({positions, List}, ChangedPosition = {position, _}) ->
    Digit = position:get_placed(ChangedPosition),
    {
      positions,
      lists:map(
	fun (Position = {position, _}) ->
		case position:is_excluded_by(Position, ChangedPosition) of
		    true ->
			position:not_possibile(Position, Digit);
		    false ->
			Position
		end
	end,
	List)
    }.

solve(Positions = {positions, _}) ->
    % We get here either because we're done, we've failed, or we have
    % to guess and recurse.  We can distinguish by examining the
    % position with the fewest possibilities remaining.

    MinPosition = min_by_possible(Positions),

    case position:get_placed(MinPosition) == undefined of
	false ->
            % Solved.  Return Positions as a solution.
	    % puts "Solved:"
	    % print_puzzle
	    {solved, Positions};
	true ->
	    Possible = position:get_possible(MinPosition),
	    case possible:size(Possible) of
		0 ->
		    % Failed.  No solution to return.
		    % puts "Backing out."
		    {failed};
		_ ->
		    % Found an unplaced position with one or more
		    % possibilities.  Guess each possibility
		    % recursively, and return any solutions we find.
		    possible:map(
		      Possible,
		      fun (Digit) ->
			      solve(place(Positions, MinPosition, Digit))
		      end)
	    end
    end.

min_by_possible({positions, List}) ->
    ByPossible =
	lists:map(
	  fun (Position = {position, _}) ->
		  Possible = position:get_possible(Position),
		  Size = possible:size(Possible),
		  {Size, Position}
	  end,
	  List),
    {_, MinPosition} = lists:min(ByPossible),
    MinPosition.
