-module(spud).
-export([maybe_update_one/2, or_else/3, do_while/3, length/1]).

% Some handy utility functions.

% Returns a function that takes a List and applies UpdateFunc to each
% element in turn until it returns {ok, NewElement}, then returns {ok,
% NewList, NewElement} where NewList has the element updated.
% Returning a list allows object:maybe_update to set the attribute to
% NewList then return both NewList and NewElement for subsequent
% use/inspection.  Returns {not_updated, List} if no element was
% updated.  Does some stunts with passing a function to itself behind
% the scenes to simulate an anonymous recursive function.
%
maybe_update_one(MaybeUpdateFunc) ->
    Func =
	fun (_Myself, []) ->
		{not_updated, []};
	    (Myself, Elements = [Element={Type,_}|Rest]) ->
		case MaybeUpdateFunc(Element) of
		    {ok, UpdatedElement} ->
		        % Once we've updated one element, leave the rest as-is.
			{ok, [UpdatedElement | Rest], UpdatedElement};
		    _ ->
			case Myself(Myself, Rest) of
			    {ok, NewElements, UpdatedElement} ->
				{ok, [Element | NewElements], UpdatedElement};
			    _ ->
				{not_updated, Elements}
			end
		end
	end,
    fun (Elements) -> Func(Func, Elements) end.


% Call Funcs on Object in turn until one of them succeeds or they all
% fail.  Retuns {ok. NewObject} or {not_updated, Object}.
%
or_else(Object = {_Type, _}, _Funcs = []) ->
    {not_updated, Object};
or_else(Object = {_Type, _}, [Func|Rest]) ->
    case Func(Object) of
	Result = {ok, _} ->
	    Result;
	_ ->
	    or_else(Object, Rest)
    end.

do_while(Object = {_Type, _}, Func) ->
    case Func(Object) of
	{ok, NewObject} ->
	    do_while(NewObject, Func);
	Result = _ ->
	    Result
    end.

length([]) ->
    0;
length([_|T]) ->
    lists:foldl(fun (_, A) -> A + 1 end, 1, T).
