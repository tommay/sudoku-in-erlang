-module(puzzle).
-export([new/1, solve/1, print_puzzle/1]).

-record(puzzle, {positions}).
-define(is_puzzle(Term), is_record(Term, puzzle)).

-include("position.hrl").

new() ->
    List = [position:new(N) || N <- lists:seq(0, 80)],
    #puzzle{positions = List}.

%% Returns a new Puzzle object which contains a list of Positions.
%% Setup is a string of 81 digits or dashes used to initialize the
%% digit placed in each Position.
%%
new(Setup) ->
    This = new(),
    Digits = to_digits(Setup),
    Zipped = lists:zip(Digits, This#puzzle.positions),
    lists:foldl(
      fun ({Digit, Position}, Accum) when ?is_puzzle(Accum)  ->
	      case Digit of
		  undefined ->
		      Accum;
		  _ ->
		      place(Accum, Position, Digit)
	      end
      end,
      This,
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

%% Returns new Puzzle with Digit placed in AtPosition.  The possible
%% sets of all Positions are updated to account for the new placement.
%%
place(This, AtPosition, Digit) when ?is_puzzle(This) ->
    AtNumber = position:get_number(AtPosition),
    This#puzzle{
      positions =
	  [case position:get_number(Position) == AtNumber of
	       true ->
		   position:place(Position, Digit);
	       false ->
		   case ?position@is_excluded_by(Position, AtPosition) of
		       true ->
			   position:not_possible(Position, Digit);
		       false ->
			   Position
		   end
	   end || Position <- This#puzzle.positions]
     }.

%% Returns a possibly empty list of solved Puzzles starting from this
%% Puzzle.
%%
solve(This) when ?is_puzzle(This) ->
    spawn_solver(This, self()),
    receive_solutions().

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Collector, and possibly spawns further processes
%% that also report back to the Collector.  Sends the Collector a
%% started message for its bookkeeping.
%%
spawn_solver(This, Collector) when ?is_puzzle(This), is_pid(Collector) ->
    Collector ! started,
    case semaphore:try_acquire(limiter) of
	true ->
	    %% Need to send "started" from the current process so the
	    %% Collector receives it before it receives our failed
	    %% message, adjusts its count, and possibly finishes.
	    spawn(fun () -> solve(This, Collector) end);
	false ->
	    solve(This, Collector)
    end.

%% Try to solve this Puzzle then report back solved or failed to the
%% Collector, and possibly spawns further processes that also report
%% back to the Collector.
%%
solve(This, Collector) when ?is_puzzle(This) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = min_by_possible_size(This),
    Possible = position:get_possible(MinPosition),
    case Possible == undefined of
	true ->
            %% Solved.  Return This as a solution.
	    Collector ! {solved, This};
	false ->
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    Collector ! failed;
		_ ->
		    %% Found an unplaced position with two or more
		    %% possibilities.  Guess each possibility and
		    %% either spawn a solver or (for the last
		    %% possibility) recurse.
		    PossibileDigitList = possible:to_list(Possible),
		    do_guesses(This, Collector,
			       MinPosition, PossibileDigitList)
	    end
    end.

%% If this is the last guess then just recurse in thie process.  If
%% there are more guesses to make then spawn a solver for this one.
%%
do_guesses(This, Collector, Position, [Digit]) ->
    Guess = place(This, Position, Digit),
    solve(Guess, Collector);
do_guesses(This, Collector, Position, [Digit|Rest]) ->
    Guess = place(This, Position, Digit),
    spawn_solver(Guess, Collector),
    do_guesses(This, Collector, Position, Rest).

%% Keep track of pending results and accumulate the solutions we've
%% gotten so far, and recurse until there are no pending results left.
%%
receive_solutions() ->
    receive_solutions(0, []).

receive_solutions(Pending, Solutions) ->
    receive
	started ->
	    stats:spawned(),
	    count(Pending, +1, Solutions);
	{solved, Puzzle} ->
	    stats:solved(),
	    count(Pending, -1, [Puzzle | Solutions]);
	failed ->
	    stats:failed(),
	    count(Pending, -1, Solutions);
	Msg = _ ->
	    io:format("wtf: ~p~n", [Msg]),
	    receive_solutions(Pending, Solutions)
    end.

count(Pending, Increment, Solutions) ->
    Pending2 = Pending + Increment,
    case Pending2 of
	0 ->
	    Solutions;
	_ ->
	    receive_solutions(Pending2, Solutions)
    end.

%% Returns the unplaced Position with the smallest set of
%% possibilities.  This is used to find the best Position to make a
%% guess for to minimize the amount of guessing.
%%
min_by_possible_size(This) when ?is_puzzle(This) ->
    spud:min_by(
      This#puzzle.positions,
      fun (Position) ->
	      Possible = position:get_possible(Position),
	      %% Sort placed positions to the end.
	      case Possible == undefined of
		  false ->
		      possible:size(Possible);
		  true ->
 		      10
	      end
      end).

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string(This) when ?is_puzzle(This) ->
    lists:map(
      fun (Position) ->
	      case position:get_placed(Position) of
		  undefined ->
		      $-;
		  Digit ->
		      Digit + $0
	      end
      end,
      This#puzzle.positions).

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

