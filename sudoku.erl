-module(sudoku).
-export([start/1]).
-compile(export_all).

s() ->
    start("../puzzle3.txt").

start(Filename) ->
    Setup = get_setup(Filename),
    Positions = positions:new(Setup),
    Solutions = positions:solve(Positions),
    lists:foreach(
      fun (SolvedPositions) ->
	      io:format("~n"),
	      positions:print_puzzle(SolvedPositions),
	      io:format("~n")
      end,
      Solutions),
    io:format("~w solutions~n", [length(Solutions)]).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
