-module(puzzle).
-export([new/1, solve/1, print_puzzle/1]).

-record(puzzle, {positions, exclusions}).
-define(is_puzzle(Term), is_record(Term, puzzle)).

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

%% Returns a new Puzzle with Digit placed at Position AtNumber.  The
%% possible sets of all Positions are updated to account for the new
%% placement.
%%
place(This, AtNumber, Digit)
  when ?is_puzzle(This), is_number(AtNumber), is_number(Digit) ->
    ExclusionList = exclusions:get_list_for_position(
		      This#puzzle.exclusions, AtNumber),
    NewPositions =
	[begin
	     Number = position:get_number(Position),
	     case Number == AtNumber of
		 true ->
		     position:place(Position, Digit);
		 false ->
		     case lists:member(Number, ExclusionList) of
			 true ->
			     position:not_possible(Position, Digit);
			 false ->
			     Position
		     end
	     end
	 end || Position <- This#puzzle.positions],
    This#puzzle{positions = NewPositions}.

%% Returns a possibly empty list of solved Puzzles starting from this
%% Puzzle.
%%
solve(This) when ?is_puzzle(This) ->
    spawn_solver(This, self()),
    receive_solutions().

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Listener, and possibly spawns further processes
%% that also report back to the Listener.  Sends the Listener a started
%% message for its bookkeeping.
%%
spawn_solver(This, Listener) when ?is_puzzle(This), is_pid(Listener) ->
    spawn(fun () -> solve(This, Listener) end),
    %% Need to send "started" from the current process so the Listener
    %% receives it before it receives our failed message, adjusts its
    %% count, and possibly finishes.
    Listener ! started.

%% Try to solve this Puzzle then report back solved or failed to the
%% Listener, and possibly spawns further processes that also report
%% back to the Listener.
%%
solve(This, Listener) when ?is_puzzle(This), is_pid(Listener) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = positions:min_by_possible_size(This#puzzle.positions),
    Possible = position:get_possible(MinPosition),

    case Possible == undefined of
	true ->
            %% Solved.  Return This as a solution.
	    Listener ! {solved, This};
	false ->
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    Listener ! failed;
		_ ->
		    %% Found an unplaced position with two or more
		    %% possibilities.  Guess each possibility and
		    %% either spawn a solver or (for the last
		    %% possibility) recurse.
		    PossibileDigitList = possible:to_list(Possible),
		    do_guesses(This, Listener,
			       position:get_number(MinPosition),
			       PossibileDigitList)
	    end
    end.

%% If this is the last guess then just recurse in thie process.  If
%% there are more guesses to make then spawn a solver for this one.
%%
do_guesses(This, Listener, Number, [Digit]) ->
    Guess = place(This, Number, Digit),
    solve(Guess, Listener);
do_guesses(This, Listener, Number, [Digit|Rest]) ->
    Guess = place(This, Number, Digit),
    spawn_solver(Guess, Listener),
    do_guesses(This, Listener, Number, Rest).

%% Keep track of pending results and accumulate the solutions we've
%% gotten so far, and recurse until there are no pending results left.
%%
receive_solutions() ->
    receive_solutions(0, []).

receive_solutions(Pending, Solutions) ->
    receive
	started ->
	    count(Pending, +1, Solutions);
	{solved, Puzzle} ->
	    count(Pending, -1, [Puzzle | Solutions]);
	failed ->
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
