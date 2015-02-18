-module(sudoku).
-export([start/1]).
-export([s/0]).

s() ->
    start("../evil1.txt").

%% Initializes Puzzle from the given Filename and prints out solutions
%% if any.
%%
start(Filename) ->
    Setup = get_setup(Filename),
    Puzzle = puzzle:new(Setup),
    Solutions = puzzle:solve(Puzzle),
    lists:foreach(
      fun (SolvedPuzzle) ->
	      io:format("~n"),
	      puzzle:print_puzzle(SolvedPuzzle),
	      io:format("~n")
      end,
      Solutions),
    io:format("~w solutions~n", [length(Solutions)]).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
