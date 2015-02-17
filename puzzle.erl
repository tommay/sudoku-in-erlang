-module(puzzle).
-export([new/1, solve/1, print_puzzle/1]).

%% Returns a new Puzzle object which contains a list of Positions.
%% Setup is a string of 81 digits or dashes used to initialize the
%% digit placed in each Position.
%%
new(Setup) ->
    List = [position:new(N) || N <- lists:seq(0, 80)],
    Puzzle = {puzzle, List},
    Digits = to_digits(Setup),
    Zipped = lists:zip(Digits, List),
    lists:foldl(
      fun ({Digit, Position = {position, _}}, Accum = {puzzle, _}) ->
	      case Digit of
		  undefined ->
		      Accum;
		  _ ->
		      place(Accum, Position, Digit)
	      end
      end,
      Puzzle,
      Zipped).

%% Given a Setup string, returns a list of numbers or undefined for
%% each position.
%%
to_digits(Setup) ->
    [case Char of
	 45 ->
	     undefined;
	 _ ->
	     Char - 48
     end || Char <- Setup].

%% Returns new Puzzle with Digit placed in AtPosition.  The possible
%% sets of all Positions are updated to account for the new placement.
%%
place({puzzle, List}, AtPosition, Digit) ->
    AtNumber = position:get_number(AtPosition),
    {
      puzzle,
      [case position:get_number(Position) == AtNumber of
	   true ->
	       position:place(Position, Digit);
	   false ->
	       case position:is_excluded_by(Position, AtPosition) of
		   true ->
		       position:not_possible(Position, Digit);
		   false ->
		       Position
	       end
       end || Position <- List]
    }.

%% Returns a possibly empty list of solved Puzzles starting from this
%% Puzzle.
%%
solve(Puzzle = {puzzle, _}) ->
    spawn_solver(self(), Puzzle),
    receive_solutions().

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Listener, and possibly spawns further processes
%% that also report back to the Listener.  Sends the Listener a started
%% message for its bookkeeping.
%%
spawn_solver(Listener, Puzzle = {puzzle, _}) ->
    Solver = spawn(fun () -> solve(Listener, Puzzle) end),
    Listener ! {Solver, started}.

%% Try to solve the Puzzle then report back solved or failed to the
%% Listener, and possibly spawns further processes that also report
%% back to the Listener.
%%
solve(Listener, Puzzle = {puzzle, _}) ->
    semaphore:acquire(limiter),

    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = min_by_possible_size(Puzzle),

    case position:get_placed(MinPosition) == undefined of
	false ->
            %% Solved.  Return Puzzle as a solution.
	    Listener ! {self(), solved, Puzzle};
	true ->
	    Possible = position:get_possible(MinPosition),
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    Listener ! {self(), failed};
		_ ->
		    %% Found an unplaced position with one or more
		    %% possibilities.  Guess each possibility, spawn
		    %% a solve process, and return any solutions we
		    %% get.
		    possible:foreach(
		      Possible,
		      fun (Digit) ->
			      Guess = place(Puzzle, MinPosition, Digit),
			      spawn_solver(Listener, Guess)
		      end),
		    Listener ! {self(), failed}
	    end
    end.

%% This uses a Dict to track each process that we're waiting for.  A
%% spawning process will send a message when it has spawned solver
%% process.  The last thing a solver process does is send a
%% solved/failed message.  The solved/failed message may arrive before
%% the started message.  Once a pair of messages has arrived for the
%% Solver (in any order) the Solver is removed from the dict.  If the
%% dict is empty we wait for more messages otherwise we return what we
%% got.
%%
%% I had to think about races a lot here when doing the bookkeeping to
%% make sure receive_solutions didn't return until all processes had
%% reported back, if we get the solved/failed message before the
%% started message.  And also to make sure started messages were sent
%% before solved/failed messages.  It was like the bad old days of
%% concurrent programming, ugh.
%%
receive_solutions() ->
    receive_solutions(dict:new(), []).

receive_solutions(Dict, Solutions) ->
    receive
	{Solver, started} ->
	    count(Dict, Solver, Solutions);
	{Solver, solved, Puzzle} ->
	    count(Dict, Solver, [Puzzle | Solutions]);
	{Solver, failed} ->
	    count(Dict, Solver, Solutions)
    end.

count(Dict, Solver, Solutions) ->
    Dict2 = case dict:find(Solver, Dict) of
		{ok, 1} ->
		    dict:erase(Solver, Dict);
		error ->
		    dict:store(Solver, 1, Dict)
	    end,
    case dict:size(Dict2) of
	0 ->
	    Solutions;
	_ ->
	    receive_solutions(Dict2, Solutions)
    end.

%% Returns the unplaced Position with the smallest set of
%% possibilities.  This is used to find the best Position to make a
%% guess for to minimize the amount of guessing.
%%
min_by_possible_size({puzzle, List}) when is_list(List) ->
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

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string({puzzle, List}) when is_list(List) ->
    lists:map(
      fun (Position = {position, _}) ->
	      case position:get_placed(Position) of
		  undefined ->
		      45;
		  Digit ->
		      Digit + 48
	      end
      end,
      List).

%% Returns a string that prints out as a grid of digits.
%%
to_puzzle(Puzzle = {puzzle, _}) ->
    String = to_string(Puzzle),
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
print_puzzle(Puzzle = {puzzle, _}) ->
    io:format("~s~n", [to_puzzle(Puzzle)]).

