-module(spud).
-export([update_one/2, or_else/3, do_while/3, length/1]).

% Some handy utility functions.

% Returns a function that takes a List and applies UpdateFunc to each
% element in turn until it returns {ok, NewElement}, then returns {ok,
% NewList, NewElement} where NewList has the element updated.
% Returning a list allows object:update to set the attribute to
% NewList then return both NewList and NewElement for subsequent
% use/inspection.  Returns {not_updated, List} if no element was
% updated.  Does some stunts with passing a function to itself behind
% the scenes to simulate an anonymous recursive function.
%
update_one(Type, UpdateFunc) ->
    Func =
	fun (_Myself, []) ->
		{not_updated, []};
	    (Myself, Elements = [Element|Rest]) ->
		case UpdateFunc(Type, Element) of
		    {ok, UpdatedElement} ->
		        % Once we've updated one element, leave the rest as-is.
			{ok, [[UpdatedElement | Rest], UpdatedElement]};
		    _ ->
			case Myself(Myself, Rest) of
			    {ok, [NewElements, UpdatedElement]} ->
				{ok, [[Element | NewElements], UpdatedElement]};
			    _ ->
				{not_updated, Elements}
			end
		end
	end,
    fun (Elements) -> Func(Func, Elements) end.


% Call Funcs on Object in turn until one of them succeeds or they all
% fail.  Retuns {ok. NewObject} or {not_updated, Object}.
%
or_else(Type, Object = {Type, _}, _Funcs = []) ->
    {not_updated, Object};
or_else(Type, Object = {Type, _}, [Func|Rest]) ->
    case Func(Type, Object) of
	Result = {ok, _} ->
	    Result;
	_ ->
	    or_else(Type, Object, Rest)
    end.

do_while(Type, Object = {Type, _}, Func) ->
    case Func(Type, Object) of
	{ok, NewObject} ->
	    do_while(Type, NewObject, Func);
	Result = _ ->
	    Result
    end.


length([]) ->
    0;
length([_|T]) ->
    lists:foldl(fun (_, A) -> A + 1 end, 1, T).

