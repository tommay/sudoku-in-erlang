-module(sudoku).
-export([start/1]).
-compile(export_all).

s() ->
    start("../puzzle3.txt").

start(Filename) ->
    Setup = get_setup(Filename),
    Positions = positions:new(Setup),
    io:format("positions: ~s~n", [positions:to_string(Positions)]),
    positions:solve(Positions).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
