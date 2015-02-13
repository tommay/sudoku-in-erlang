-module(spud).
-export([maybe_update_one/2, or_else/3, do_while/3, length/1]).

% Some handy utility functions.

% Strip the type off and just deal with the List, then add the Type back.
%
maybe_update_one(Data = {Type, List}, MaybeUpdateFunc) ->
    case maybe_update_one(List, MaybeUpdateFunc) of
	{ok, NewList, ChangedElement} ->
	    {ok, {Type, NewList}, ChangedElement};
	_ ->
	    {not_updated, Data}
    end.

maybe_update_one(List = [], _MaybeUpdateFunc) ->
    {not_updated, List};

maybe_update_one(List = [First|Rest]}, MaybeUpdateFunc) ->
    case MaybeUpdateFunc(First) of
	{ok, UpdatedElement} ->
	    % Once we've updated one element, leave the rest as-is.
	    {ok, [UpdatedElement | Rest], UpdatedElement};
	_ ->
	    case maybe_update_one(Rest, MaybeUpdateFunc) of
		{ok, NewList, UpdatedElement} ->
		    {ok, [First | NewList], UpdatedElement};
		_ ->
		    {not_updated, List}
	    end
    end.

% Call Funcs on Object in turn until one of them succeeds or they all
% fail.  Retuns {ok. NewObject} or {not_updated, Object}.
%
or_else(Object = {_Type, _}, _MaybeFuncs = []) ->
    {not_updated, Object};
or_else(Object = {_Type, _}, [MaybeFunc|Rest]) ->
    case MaybeFunc(Object) of
	Result = {ok, _} ->
	    Result;
	_ ->
	    or_else(Object, Rest)
    end.

do_while(Object = {_Type, _}, MaybeFunc) ->
    case MaybeFunc(Object) of
	{ok, NewObject} ->
	    do_while(NewObject, Func);
	Result = _ ->
	    Result
    end.

length([]) ->
    0;
length([_|T]) ->
    lists:foldl(fun (_, A) -> A + 1 end, 1, T).
