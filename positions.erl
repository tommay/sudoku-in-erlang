-module(positions).
-export([new/1, maybe_update_one_forced/1]).

new(Setup) ->
    Digits = to_digits(Setup),
    List = [position:new(N) || N <- lists:seq(0, 80)],
    Positions = {positions, List},
    Zipped = lists:zip(List, Digits),
    {
      positions,
      lists:foldl(
	fun ({Digit, Position = {position, _}}, Accum) ->
		case object:get(Position, placed) of
		    undefined ->
			Accum;
		    _ ->
			place(accum, Position, Digit)
		end
	end,
	Positions,
	Zipped)
      }.

to_digits(Setup) ->
    lists:map(
      fun (Char) ->
	      case Char of
		  45 ->
		      undefined;
		  _ ->
		      Char - 48
	      end
      end,
      Setup).

place(Positions = {positions, List}, Position, Digit) ->
    PlacedPosition = object:set(Position, placed, Digit),
    Number = object:get(PlacedPosition, number),
    NewPositions = {
      positions,
      [case object.get(P, number) == Number of
	   true -> PlacedPosition;
	   false -> P
       end || P <- List]},
    do_exclusions(NewPositions, PlacedPosition).

% Returns {ok, NewPositions} if successful.
%
maybe_update_one_forced(Positions = {positions, _}) ->
    maybe_update_one(Positions, fun position:maybe_place_forced/1).

% Returns {ok, NewPositions} if successful.
%
maybe_update_one(Positions = {positions, _}, MaybeUpdateFunc) ->
    case spud:maybe_update_one(Positions, MaybeUpdateFunc) of
	{ok, NewPositions, ChangedPosition} ->
	    {ok, do_exclusions(NewPositions, ChangedPosition)};
	Result = _ ->
	    Result
    end.

# Returns NewPositions.
#
do_exclusions({positions, List}, ChangedPosition = {position, _}) ->
    Digit = object:get(ChangedPosition, placed),
    {
      positions,
      lists:map(
	fun (Position = {position, _}) ->
		case position:is_excluded_by(Position, ChangedPosition) of
		    true ->
			position:not_possibile(Position, Digit);
		    false ->
			Position
		end
	end,
	List)
    }.
