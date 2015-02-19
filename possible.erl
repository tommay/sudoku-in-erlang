-module(possible).
-export([new/0, remove/2, size/1, to_list/1]).
-export([to_string/1]).

%% An object that maintains the possible digits for a position.

new() ->
    {possible, sets:from_list(lists:seq(1, 9))}.

remove({possible, Set}, Digit) ->
    {possible, sets:del_element(Digit, Set)}.

size({possible, Set}) ->
    sets:size(Set).

to_list({possible, Set}) ->
    sets:to_list(Set).

to_string({possible, Set}) ->
    spud:format("~w", [sets:to_list(Set)]).

