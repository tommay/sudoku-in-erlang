-module(puzzle).
-include("puzzle.hrl").
-export([new/1, foreach_solution/2, print_puzzle/1]).

%% Returns a new Puzzle with empty Positions.
%%
new() ->
    #puzzle{positions = positions:new(),
	    exclusions = exclusions:new()}.

%% Returns a new Puzzle with each Position initialized according to
%% Setup, which is a string of 81 digits or dashes.
%%
new(Setup) ->
    Digits = to_digits(Setup),
    Numbers = lists:seq(0, 80),
    Zipped = lists:zip(Digits, Numbers),
    lists:foldl(
      fun ({Digit, Number}, This) ->
	      case Digit of
		  undefined ->
		      This;
		  _ ->
		      place(This, Number, Digit)
	      end
      end,
      new(),
      Zipped).

%% Given a Setup string, returns a list of numbers or undefined for
%% each position.
%%
to_digits(Setup) ->
    [case Char of
	 $- ->
	     undefined;
	 _ ->
	     Char - $0
     end || Char <- Setup].

%% Solve this Puzzle and call Yield with each solved Puzzle.
%%
foreach_solution(This, Yield) when ?is_puzzle(This), is_function(Yield) ->
    Collector = self(),
    spawn_solver(This, Collector),
    collector:collect_and_yield_results(
      fun (Result) -> yield_solutions(Result, Yield) end).

spawn_solver(Puzzle, Collector) when ?is_puzzle(Puzzle), is_pid(Collector) ->
    stats:spawned(),
    collector:spawn_solver(Collector, fun () -> solve(Puzzle, Collector) end).

yield_solutions({solved, Puzzle}, Yield) when ?is_puzzle(Puzzle) ->
    stats:solved(),
    Yield(Puzzle);
yield_solutions(failed, _Yield) ->
    stats:failed().

%% Try to solve this Puzzle then report back solved or failed to the
%% Collector, and possibly spawn further processes that also report
%% back to the Collector.
%%
solve(This, Collector) when ?is_puzzle(This), is_pid(Collector) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = positions:min_by_possible_size(This#puzzle.positions),
    Possible = position:get_possible(MinPosition),

    case Possible == undefined of
	true ->
            %% Solved.  Return This as a solution.
	    collector:yield(Collector, {solved, This});
	false ->
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    collector:yield(Collector, failed);
		_ ->
		    %% Found an unplaced position with two or more
		    %% possibilities.  Guess each possibility and
		    %% either spawn a solver or (for the last
		    %% possibility) recurse.
		    PossibileDigitList = possible:to_list(Possible),
		    do_guesses(This, Collector,
			       position:get_number(MinPosition),
			       PossibileDigitList)
	    end
    end.

%% Fpr each Digit in the list, use it as a guess for Position Number
%% and try to solve the resulting Puzzle.
%%
do_guesses(This, Collector, Number, [Digit|Rest]) ->
    Guess = puzzle:place(This, Number, Digit),
    %% If this is the last guess then just solve in this process.  If
    %% there are more guesses to make then spawn a solver for this one
    %% and recurse to process the rest.
    case Rest of
	[] ->
	    solve(Guess, Collector);
	_ ->
	    spawn_solver(Guess, Collector),
	    do_guesses(This, Collector, Number, Rest)
    end.

%% Returns a new Puzzle with Digit placed at Position AtNumber.  The
%% possible sets of all Positions are updated to account for the new
%% placement.
%%
place(This, AtNumber, Digit)
  when ?is_puzzle(This), is_number(AtNumber), is_number(Digit) ->
    Positions = This#puzzle.positions,
    %% Place the Digit.
    Positions2 = positions:update(
		   Positions,
		   AtNumber,
		   fun (Position) -> position:place(Position, Digit) end),
    %% Exclude Digit from excluded Positions.
    ExclusionList = exclusions:get_list_for_position(
		      This#puzzle.exclusions, AtNumber),
    Positions3 = positions:do_exclusions(Positions2, Digit, ExclusionList),
    This#puzzle{positions = Positions3}.

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string(This) when ?is_puzzle(This) ->
    positions:to_string(This#puzzle.positions).

%% Returns a string that prints out as a grid of digits.
%%
to_puzzle(This) when ?is_puzzle(This) ->
    String = to_string(This),
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

%% Prints the to_puzzle string.
%%
print_puzzle(This) when ?is_puzzle(This) ->
    io:format("~s~n", [to_puzzle(This)]).
