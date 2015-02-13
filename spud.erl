-module(spud).
-export([maybe_update_one/2, or_else/3, do_while/3, length/1]).

% Some handy utility functions.

maybe_update_one(_Elements = [], _MaybeUpdateFunc) ->
    {not_updated, Elements};
maybe_update_one(Elements = [Element={Type,_}|Rest], MaybeUpdateFunc) ->
    case MaybeUpdateFunc(Element) of
	{ok, UpdatedElement} ->
	    % Once we've updated one element, leave the rest as-is.
	    {ok, [UpdatedElement | Rest], UpdatedElement};
	_ ->
	    case maybe_update_one(Rest, MaybeUpdateFunc) of
		{ok, NewElements, UpdatedElement} ->
		    {ok, [Element | NewElements], UpdatedElement};
		_ ->
		    {not_updated, Elements}
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
