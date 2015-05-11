-module(sudoku).
-export([main/1]).

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
      fun (_SolvedPuzzle) ->
	      puzzle:print_puzzle(_SolvedPuzzle),
	      io:format("~n~n")
      end),
%%    io:format("~w solutions~n", [length(Solutions)]),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
