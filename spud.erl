-module(spud).
-export([maybe_update_one/2, or_else/2, do_while/2, min_by/2, slices/2]).
-export([format/2, debug/1, debug/2]).

%% Some handy utility functions.

%% Strip the type off and just deal with the List, then add the Type back.
%%
maybe_update_one(Data = {Type, List}, MaybeUpdateFunc) ->
    case maybe_update_one(List, MaybeUpdateFunc) of
	{ok, NewList, ChangedElement} ->
	    {ok, {Type, NewList}, ChangedElement};
	_ ->
	    {not_updated, Data}
    end;

maybe_update_one(List = [], _MaybeUpdateFunc) ->
    {not_updated, List};

maybe_update_one(List = [First|Rest], MaybeUpdateFunc) ->
    case MaybeUpdateFunc(First) of
	{ok, UpdatedElement} ->
	    %% Once we've updated one element, leave the rest as-is.
	    {ok, [UpdatedElement | Rest], UpdatedElement};
	_ ->
	    case maybe_update_one(Rest, MaybeUpdateFunc) of
		{ok, NewList, UpdatedElement} ->
		    {ok, [First | NewList], UpdatedElement};
		_ ->
		    {not_updated, List}
	    end
    end.

%% Call Funcs on Object in turn until one of them succeeds or they all
%% fail.  Retuns {ok. NewObject} or {not_updated, Object}.
%%
or_else(Object = {_Type, _}, _MaybeFuncs = []) ->
    {not_updated, Object};
or_else(Object = {_Type, _}, [MaybeFunc|Rest]) ->
    case MaybeFunc(Object) of
	Result = {ok, _} ->
	    Result;
	_ ->
	    or_else(Object, Rest)
    end.

do_while(Object = {_Type, _}, MaybeFunc)
  when is_atom(_Type), is_function(MaybeFunc) ->
    case MaybeFunc(Object) of
	{ok, NewObject} ->
	    do_while(NewObject, MaybeFunc);
	Result = _ ->
	    Result
    end.

%% Returns the element of List with the minimum value computed by Func.
%%
min_by(List, Func) when is_list(List), is_function(Func) ->
    %% This creates an entire new list.  Could use foldl to avoid that
    %% and just remember the min on the fly.  But this is simple.
    Tuples = [{Func(Element), Element} || Element <- List],
    {_, MinElement} = lists:min(Tuples),
    MinElement.


slices([], _N) when is_integer(_N) ->
    [];
slices(List, N) ->
    {Slice, Rest} = lists:split(N, List),
    [Slice | slices(Rest, N)].

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

debug(Format, Data) ->
    io:format(Format, Data).

debug(String) ->
    io:format(String).
