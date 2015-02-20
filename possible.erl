-module(possible).
-export([new/0, remove/2, size/1, to_list/1]).
-export([to_string/1]).

-record(possible, {list}).
-define(is_possible(Term), is_record(Term, possible)).

%% An object that maintains the possible digits for a position.

new() ->
    #possible{list = lists:seq(1, 9)}.

remove(This, Digit) ->
    This#possible{list = lists:delete(Digit, This#possible.list)}.

size(This) ->
    list_size(This#possible.list).

list_size([]) ->
    0;
list_size([_|T]) ->
    1 + list_size(T).

to_list(This) ->
    This#possible.list.

to_string(This) ->
    spud:format("~w", [to_list(This)]).

