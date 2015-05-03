-module(solver).
-include("puzzle.hrl").
-export([spawn_solver/2, receive_solutions/1]).

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Collector, and possibly spawns further processes
%% that also report back to the Collector.  Sends the Collector a
%% started message for its bookkeeping.
%%
spawn_solver(Puzzle, Collector) when ?is_puzzle(Puzzle), is_pid(Collector) ->
    %% Need to send "started" from the current process so the
    %% Collector receives it before it receives our failed
    %% message, adjusts its count, and possibly finishes.
    Collector ! started,
    limiter:run(limiter, fun () -> solve(Puzzle, Collector) end).

%% Try to solve this Puzzle then report back solved or failed to the
%% Collector, and possibly spawns further processes that also report
%% back to the Collector.
%%
solve(Puzzle, Collector) when ?is_puzzle(Puzzle), is_pid(Collector) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = positions:min_by_possible_size(Puzzle#puzzle.positions),
    Possible = position:get_possible(MinPosition),

    case Possible == undefined of
	true ->
            %% Solved.  Return Puzzle as a solution.
	    Collector ! {solved, Puzzle};
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
		    do_guesses(Puzzle, Collector,
			       position:get_number(MinPosition),
			       PossibileDigitList)
	    end
    end.

%% If this is the last guess then just recurse in thie process.  If
%% there are more guesses to make then spawn a solver for this one.
%%
do_guesses(Puzzle, Collector, Number, [Digit]) ->
    Guess = puzzle:place(Puzzle, Number, Digit),
    solve(Guess, Collector);
do_guesses(Puzzle, Collector, Number, [Digit|Rest]) ->
    Guess = puzzle:place(Puzzle, Number, Digit),
    spawn_solver(Guess, Collector),
    do_guesses(Puzzle, Collector, Number, Rest).

%% Keep track of pending results and call Yield with solutions as they
%% are found.  Loop until there are no pending results left.  Note
%% that we start with no pending results.
%%
receive_solutions(Yield) ->
    receive_solutions(0, Yield).

receive_solutions(Pending, Yield) ->
    receive
	started ->
	    stats:spawned(),
	    receive_solutions(Pending + 1, Yield);
	{solved, Puzzle} ->
	    stats:solved(),
	    Yield(Puzzle),
	    maybe_receive_solutions(Pending - 1, Yield);
	failed ->
	    stats:failed(),
	    maybe_receive_solutions(Pending - 1, Yield);
	Msg ->
	    io:format("wtf: ~p~n", [Msg]),
	    receive_solutions(Pending, Yield)
    end.

maybe_receive_solutions(0, _Yield) ->
    ok;
maybe_receive_solutions(Pending, Yield) ->
    receive_solutions(Pending, Yield).
