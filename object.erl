-module(object).
-export([new/1, new/2, set/3, set/2, get/2, update/3]).

%% Support "objects" as {type, Dictionary}, where type is an atom and
%% Dictionary is a dict:new().  Including the type provides runtime
%% type checking and makes it easy to see what type an object is.
%%
%% Using a dictionary is a lot like JavaScript.  But I don't (yet)
%% keep methods in the dictionary, just attributes.  And there is no
%% inheritance.  Methods are defined as top-level functions in their
%% respective source files.  This seems fine; it's simple and clean.

%% Returns a new "object" of the given type with no attributes set.
%%
new(Type) when is_atom(Type)->
    {Type, dict:new()}.

%% Returns a new "object" of the given type with attributes set.
%% Attributes is a list of {attribute, Value} tuples to initialize the
%% new object with.
%%
new(Type, Attributes) when is_atom(Type), is_list(Attributes) ->
    NewObject = new(Type),
    set(NewObject, Attributes).

%% Returns the Value of the Attribute.
%%
get({Type, Dict}, Attribute) when is_atom(Type), is_atom(Attribute) ->
    dict:fetch(Attribute, Dict).

%% Returns NewObject.
%%
set(_Object = {Type, Dict}, Attribute, Value)
  when is_atom(Type), is_atom(Attribute) ->
    {Type, dict:store(Attribute, Value, Dict)}.

%% Returns NewObject, with the list of {Attribute, Value} set.
%%
set(Object = {Type, _Dict}, []) when is_atom(Type) ->
    Object;
set(Object = {Type, _Dict}, [{Attribute, Value} | T])
  when is_atom(Type), is_atom(Attribute) ->
    NewObject = set(Object, Attribute, Value),
    set(NewObject, T);
set(Object = {Type, _Dict}, [Attribute | T])
  when is_atom(Type), is_atom(Attribute) ->
    NewObject = set(Object, Attribute, undefined),
    set(NewObject, T).

%% Returns NewObject.
%%
update(_Object = {Type, Dict}, Attribute, UpdateFunc) ->
    {Type, dict:update(Attribute, UpdateFunc, Dict)}.
