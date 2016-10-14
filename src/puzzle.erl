-module(puzzle).
-export([new/1, foreach_solution/2, to_puzzle_string/1]).

-include("puzzle.hrl").
-include("unknown.hrl").

%% Returns a new Puzzle with empty Cells.
%%
new() ->
    #puzzle{
       placed = [],
       unknown = [unknown:new(N) || N <- lists:seq(0, 80)]
      }.

%% Returns a new Puzzle with each Cell initialized according to
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
%% each cell.
%%
to_digits(Setup) ->
    [case Char of
	 $- ->
	     undefined;
	 _ ->
	     Char - $0
     end || Char <- Setup].

%% Solve this Puzzle and call Yield with each solved Puzzle.
%% XXC The solver sends solved puzzles asynchornously and can send them
%% faster than Yield can process them which uses unbounded memory until
%% the process is killed.
%%
foreach_solution(This, Yield) when ?is_puzzle(This), is_function(Yield) ->
    Collector = self(),
    spawn_solver(This, Collector),
    collector:collect_and_yield_results(
      fun (Result) ->
	      case Result of
		  {solved, Puzzle} ->
		      stats:solved(),
		      Yield(Puzzle);
		  failed ->
		      stats:failed()
	      end
      end).

spawn_solver(Puzzle, Collector) when ?is_puzzle(Puzzle), is_pid(Collector) ->
    stats:spawned(),
    collector:spawn_solver(Collector, fun () -> solve(Puzzle, Collector) end).

%% Try to solve this Puzzle then report back solved or failed to the
%% Collector, and possibly spawn further processes that also report
%% back to the Collector.
%%
solve(This, Collector) when ?is_puzzle(This), is_pid(Collector) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced cell with the fewest possibilities remaining.

    case This#puzzle.unknown of
	[] ->
            %% Solved.  Return This as a solution.
	    collector:yield(Collector, {solved, This});
	Unknowns ->
	    MinUnknown = unknown:min_by_num_possible(Unknowns),
	    Possible = unknown:possible(MinUnknown),

	    case Possible of
		[] ->
		    %% Failed.  Return no solutions.
		    collector:yield(Collector, failed);
		_ ->
		    %% Found an unplaced cell with two or more
		    %% possibilities.  Guess each possibility and
		    %% either spawn a solver or (for the last
		    %% possibility) recurse.
		    do_guesses(This, Collector,
			       unknown:cell_number(MinUnknown),
			       Possible)
	    end
    end.

%% Fpr each Digit in the list, use it as a guess for Cell Number
%% and try to solve the resulting Puzzle.
%%
do_guesses(This, Collector, Number, [Digit|Rest]) ->
    Guess = place(This, Number, Digit),
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

%% Returns a new Puzzle with Digit placed in Cell CellNumber.  The
%% possible sets of all Cells are updated to account for the new
%% placement.
%%
place(This, CellNumber, Digit)
  when ?is_puzzle(This), is_number(CellNumber), is_number(Digit) ->
    place(This, unknown:new(CellNumber), Digit);
place(This, Unknown, Digit)
  when ?is_puzzle(This), ?is_unknown(Unknown), is_number(Digit) ->
    CellNumber = unknown:cell_number(Unknown),
    Placed = [{CellNumber, Digit} | This#puzzle.placed],
    Unknown2 = lists:filtermap(
		 fun (E) ->
			 case unknown:cell_number(E) /= CellNumber of
			     true -> {true, unknown:place(E, Unknown, Digit)};
			     false -> false
			 end
		 end,
		 This#puzzle.unknown),
    This#puzzle{placed = Placed, unknown = Unknown2}.

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string(This) when ?is_puzzle(This) ->
    Placed = [{Number, $0 + Digit} || {Number, Digit} <- This#puzzle.placed],
    Unknown = [{unknown:cell_number(U), $-} || U <- This#puzzle.unknown],
    All = Placed ++ Unknown,
    Sorted = lists:sort(All),
    [Char || {_, Char} <- Sorted].

%% Returns a string that prints out as a grid of digits.
%%
to_puzzle_string(This) when ?is_puzzle(This) ->
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
