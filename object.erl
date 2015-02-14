-module(object).
-export([new/1, new/2, set/3, get/2, update/3, maybe_update/3]).

% Support objects as {type, Dictionary}, where type is an atom and
% Dictionary is a dict:new().  Including the type provides runtime
% type checking and makes it easy to see what type an object is.
%
% Using a dictionary is a lot like JavaScript.  But I don't (yet) keep
% methods in the dictionary, just attributes.  And there is no
% inheritance.  Methods are defined as top-level functions in their
% respective source files.

new(Type) when is_atom(Type)->
    {Type, dict:new()}.

new(Type, Attributes) when is_atom(Type) ->
    {Type,
     lists:foldl(
       fun ({Attribute, Value}, Object) ->
	       set(Object, Attribute, Value);
	   (Attribute, Object) ->
	       set(Object, Attribute, undefined)
       end,
       new(Type),
       Attributes)}.

get({Type, Dict}, Attribute) when is_atom(Type) and is_atom(Attribute) ->
    case dict:is_dict(Dict) of
	true -> dict:fetch(Attribute, Dict)
    end.

set({Type, Dict}, Attribute, Value) when is_atom(Type) and is_atom(Attribute) ->
    case dict:is_dict(Dict) of
	true -> {Type, dict:update(Attribute, Dict, Value)}
    end.

% MaybeUpdateFunc(Value) takes the attribute's current Value and
% returns {ok, NewValue} if the attribute should be updated, or
% something else if it shouldn't.
%
% Returns {ok, NewObject} if the attribute was
% updated, otherwise {not_updated, Object}.
%
maybe_update(Object = {Type, _}, Attribute, MaybeUpdateFunc)
  when is_atom(Type) and is_atom(Attribute) and is_function(MaybeUpdateFunc) ->
    Value = get(Object, Attribute),
    case MaybeUpdateFunc(Value) of
	{ok, NewValue} ->
	    {ok, set(Object, Attribute, NewValue)};
	_ ->
	    {not_updated, Object}
    end.

% Returns NewObject.
%
update(Object = {_Type, _}, Attribute, UpdateFunc) ->
    set(Object, Attribute, UpdateFunc(get(Object, Attribute))).
