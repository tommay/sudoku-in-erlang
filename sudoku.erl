-module(sudoku).

Puzzle:
{positions, [...]},
{exclusion_sets, [...]}

positions = set_position(positions, n, new_position(...))

% What I really want is an ugly old record:

-module(position).
-record(position, {number, possible = [], placed}).

Position#position.number




-module(position)

get_val(List, Atom) ->
    case List of
	[{Atom, Val}|_] -> Val;
	[_|T] -> find(T, Atom)
    end.

number(Position) ->
    get_val(Position, number).

% This looks like a PITA.  When we want to update something about a
% position we have to dive down through the Puzzle to the thing we
% want to update and build it all back into a new Puzzle.  So we can't
% update a Position without knowing it's part of the Puzzle.

set_position(Positions, Number, New_position) ->
    map(Positions,
	fun (Position) ->
		case position:number(Position) == Number
		    % Need to make sure the position's number is set
		    % correctly.  Do we need a set_val function?
		    true -> New_position;
		    false -> Position
		end
	end).
			 



Position:
{number, Number},

