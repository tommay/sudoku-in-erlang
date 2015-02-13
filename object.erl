-module(object).
-export([new/1, new/2, set/3, get/2, update/3]).

% Support objects as {type, Dictionary}, where type is an atom and
% Dictionary is a dict:new().  Including the type provides runtime
% type checking and makes it easy to see what type an object is.
%
% Using a dictionary is a lot like JavaScript.  But I don't (yet) keep
% methods in the dictionary, just attributes.  And there is no
% inheritance.  Methods are defined as top-level functions in their
% respective source files.

new(Type) ->
    {Type, dict:new()}.

new(Type, Attributes) ->
    {Type,
     lists:foldl(
       fun ({Attribute, Value}, Object) ->
	       set(Object, Attribute, Value);
	   (Attribute, Object) ->
	       set(Object, Attribute, undefined)
       end,
       new(Type),
       Attributes)}.

get({_, Dict}, Attribute) ->
    dict:fetch(Attribute, Dict).

set({Type, Dict}, Attribute, Value) ->
    {Type, dict:update(Attribute, Dict, Value)}.

% UpdateFunc(Value) takes the attribute's current Value and returns
% {ok, NewValue} if the attribute should be updated, or something else
% if it shouldn't.  If the attribute value is a list, UpdateFunc can
% optionally return {ok, NewValue, ChangedElement}.
%
% Returns {ok, NewObject [, ChangedElement]} if the attribute was
% updated, otherwise {not_updated, Object}.
%
update(Object = {_Type, _}, Attribute, UpdateFunc) ->
    Value = get(Object, Attribute),
    case UpdateFunc(Value) of
	{ok, NewValue} ->
	    {ok, set(Object, Attribute, NewValue)};
	{ok, [NewValue, ChangedElement]} ->
	    {ok, [set(Object, Attribute, NewValue), ChangedElement]};
	_ ->
	    {not_updated, Object}
    end.
