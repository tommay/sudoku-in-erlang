-module(positions).
-export([new/1, solve/1]).
-compile(export_all).

new(Setup) ->
    List = [position:new(N) || N <- lists:seq(0, 80)],
    Positions = {positions, List},
    Digits = to_digits(Setup),
    Zipped = lists:zip(Digits, List),
    lists:foldl(
      fun ({Digit, Position = {position, _}}, Accum = {positions, _}) ->
	      case Digit of
		  undefined ->
		      Accum;
		  _ ->
		      place(Accum, Position, Digit)
	      end
      end,
      Positions,
      Zipped).

to_digits(Setup) ->
    [case Char of
	 45 ->
	     undefined;
	 _ ->
	     Char - 48
     end || Char <- Setup].

place({positions, List}, AtPosition, Digit) ->
    AtNumber = position:get_number(AtPosition),
    {
      positions,
      [case position:get_number(Position) == AtNumber of
	   %% XXX set possible to just Digit?  Or undefined?
	   true ->
	       object:set(Position, placed, Digit);
	   false ->
	       case position:is_excluded_by(Position, AtPosition) of
		   true ->
		       position:not_possible(Position, Digit);
		   false ->
		       Position
	       end
       end || Position <- List]
    }.

%% Returns {ok, NewPositions} if successful.
%%
maybe_update_one_forced(Positions = {positions, _}) ->
    maybe_update_one(Positions, fun position:maybe_place_forced/1).

%% Returns {ok, NewPositions} if successful.
%%
maybe_update_one(Positions = {positions, _}, MaybeUpdateFunc) ->
    case spud:maybe_update_one(Positions, MaybeUpdateFunc) of
	{ok, NewPositions, ChangedPosition} ->
	    {ok, do_exclusions(NewPositions, ChangedPosition)};
	Result = _ ->
	    Result
    end.

%% Returns NewPositions.
%% XXX This hopefully won't be needed now that place() updates
%% possibilities.
%%
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
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = min_by_possible_size(Positions),

    case position:get_placed(MinPosition) == undefined of
	false ->
            %% Solved.  Return Positions as a solution.
	    spud:debug("Solved:~n~s~n~n", [to_puzzle(Positions)]),
	    [Positions];
	true ->
	    Possible = position:get_possible(MinPosition),
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  No solution to return.
		    spud:debug("Backing out."),
		    [];
		_ ->
		    %% Found an unplaced position with one or more
		    %% possibilities.  Guess each possibility
		    %% recursively, and return any solutions we find.
		    possible:flatmap(
		      Possible,
		      fun (Digit) ->
			      Guess = place(Positions, MinPosition, Digit),
			      solve(Guess)
		      end)
	    end
    end.

min_by_possible_size({positions, List}) when is_list(List) ->
    spud:min_by(
      List,
      fun (Position = {position, _}) ->
	      %% Sort placed positions to the end.
	      case position:get_placed(Position) of
		  undefined ->
		      Possible = position:get_possible(Position),
		      possible:size(Possible);
		  _ ->
 		      10
	      end
      end).

to_string({positions, List}) when is_list(List) ->
    lists:map(
      fun (Position = {position, _}) ->
	      case position:get_placed(Position) of
		  undefined ->
		      45;
		  Digit = _ ->
		      Digit + 48
	      end
      end,
      List).

to_puzzle(Positions = {positions, _}) ->
    String = to_string(Positions),
    string:join(
      lists:map(
	fun (Rows) ->
		string:join(
		  lists:map(
		    fun (Row) ->
			    string:join(spud:slices(Row, 3), " ")
		    end,
		    spud:slices(Rows, 9)),
		  "\n")
	end,
	spud:slices(String, 27)),
      "\n\n").

print_puzzle(Positions = {positions, _}) ->
    io:format("~s~n", [to_puzzle(Positions)]).

