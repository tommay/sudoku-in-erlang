-module(sudoku).
-export([main/1]).

%% This is the main function, called from the sudoku script.
%% Initializes Puzzle from the given Filename and prints out solutions
%% if any.
%%
main(Filename) ->
    application:start(stats),
    application:start(limiter),
    Setup = get_setup(Filename),
    Puzzle = puzzle:new(Setup),
    puzzle:foreach_solution(
      Puzzle,
      fun (SolvedPuzzle) ->
	      puzzle:print_puzzle(SolvedPuzzle),
	      io:format("~n~n")
      end),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]).

%% Returns the contents of Filename as a String with "#" comments and
%% whitespace deleted.  The result should be a string of 81 digits or
%% dashes, where the digits are given by the puzzle and the dash
%% positions are to be solved for.
%%
get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
