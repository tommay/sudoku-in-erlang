-module(object).
-export([new/1, new/2, set/4, get/3, update/4]).

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
	       set(Type, Object, Attribute, Value);
	   (Attribute, Object) ->
	       set(Type, Object, Attribute, undefined)
       end,
       new(Type),
       Attributes)}.

get(Type, {Type, Dict}, Attribute) ->
    dict:fetch(Attribute, Dict).

set(Type, {Type, Dict}, Attribute, Value) ->
    {Type, dict:update(Attribute, Dict, Value)}.

% UpdateFunc(Value) takes the attribute's current Value and returns
% {ok, NewValue} if the attribute should be updated, or something else
% if it shouldn't.
%
% Returns {ok, NewObject} if the attribute was updated, otherwise
% {not_updated, Object}.
%
update(Type, Object = {Type, _}, Attribute, UpdateFunc) ->
    Value = get(Type, Object, Attribute),
    case UpdateFunc(Value) of
	{ok, NewValue} ->
	    {ok, set(Type, Object, Attribute, NewValue)};
	_ ->
	    {not_updated, Object}
    end.
