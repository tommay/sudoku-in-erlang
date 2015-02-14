-module(possible).
-export([new/0, remove/2, size/1, first/1, map/2]).

% An object that maintains the possible digits for a position.

new() ->
    {possible, sets:from_list(lists:seq(0, 8))}.

remove({possible, Set}, Digit) ->
    {possible, sets:del_element(Set, Digit)}.

size({possible, Set}) ->
    sets:size(Set).

first({possible, Set}) ->
    hd(sets:to_list(Set)).

map({possible, Set}, Func) ->
    List = sets:to_list(Set),
    lists:map(Func, List).
