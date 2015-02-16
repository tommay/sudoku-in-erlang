#!/usr/bin/env escript

%% escript:
%% http://erlangcentral.org/frame/?href=http%3A%2F%2Fwww.erlang.org%2Fdoc%2Fman%2Fescript.html#.VOF5BS4YMvg
%%
%% erl:
%% https://erlangcentral.org/wiki/index.php?title=Running_Erlang_Code_From_The_Command_Line

%% These two lines are optional.
-module(sudoku).
-export([main/1]).

main([]) ->
    usage();
main([Filename]) ->
    start(Filename);
main(_) ->
    usage().

usage() ->
    io:format("Usage: sudoku filename~n").

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
