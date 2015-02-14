-module(possible).
-export([new/0]).

% An object that maintains the possible digits for a position.

new() ->
    {possible, sets:from_list(lists:seq(0, 8))}.

remove({possible, Set}, Digit) ->
    {possible, sets:del_element(Set, Digit)}.

size({possible, Set}) ->
    sets:size(Set).

first({possible, Set}) ->
    hd(sets:to_list(Set)).
