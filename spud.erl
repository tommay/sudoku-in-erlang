-module(spud).
-export([min_by/2, slices/2]).
-export([format/2, debug/1, debug/2]).

%% Some handy utility functions.

%% Returns the element of List with the minimum value computed by Func.
%%
min_by(List, Func) when is_list(List), is_function(Func) ->
    %% This creates an entire new list.  Could use foldl to avoid that
    %% and just remember the min on the fly.  But this is simple.
    Tuples = [{Func(Element), Element} || Element <- List],
    {_, MinElement} = lists:min(Tuples),
    MinElement.

%% Returns a list containing elements each containing N elements from
%% the original list.
%%
slices([], _N) when is_integer(_N) ->
    [];
slices(List, N) ->
    {Slice, Rest} = lists:split(N, List),
    [Slice | slices(Rest, N)].

%% Returns a string with Data formatted by Format.
%%
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% Write some information for debugging.
%%
debug(Format, Data) ->
    debug(format(Format, Data)).

%% Write a string for debugging.
%%
debug(String) ->
    String.
%    io:format(String).
