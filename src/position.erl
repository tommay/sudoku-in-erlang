-module(position).
-export([new/1, get_number/1, get_possible/1, place/2, get_placed/1]).
-export([is_excluded_by/2, not_possible/2]).
-export([to_string/1]).

%% A Position records the state of an individual sudoku square.
%% They're not called "squares" because that's used for the squares
%% that contain nine postitions.  A Position knows:
%% - its number from 0 to 80.  Positions are numbered across the board
%%   from left to right and top to bottom.
%% - its row.  Rows are numbered 0 to 8 from top to bottom.
%% - its column.  Rows are numbered 0 to 8 from left to right.
%% - its square.  Squares are numbered across the board
%%   from left to right and top to bottom.
%% - which digits are Possible to place at this Position, i.e., which
%%   digits don't conflict with previously placed digits in other
%%   Positions in the same row/column/square.  If placed is set, then
%%   possible is undefined.
%% - which digit has been placed in this Position, or undefined if no
%%   digit has been placed yet.

-record(position, {number, row, col, square, possible, placed}).
-define(is_position(Term), is_record(Term, position)).

%% Returns a new Position at position Number.  Determine the
%% Position's row, column, and square, create a new Possible with all
%% digits possible, and set placed to undefined.
%%
new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    #position{number = Number, row = Row, col = Col, square = Square,
	      possible = possible:new(), placed = undefined}.

get_number(This) when ?is_position(This) ->
    This#position.number.

get_possible(This) when ?is_position(This) ->
    This#position.possible.

place(This, Digit) when ?is_position(This) ->
    This#position{placed = Digit, possible = undefined}.

get_placed(This) when ?is_position(This) ->
    This#position.placed.

%% Returns true if This and Other are in the same row, column, or
%% square, else false.
%%
is_excluded_by(This, Other)
  when ?is_position(This), ?is_position(Other) ->
    %% Shouldn't have to check that it's the same Position.
    get_number(This) /= get_number(Other) andalso
	(This#position.row == Other#position.row orelse
	 This#position.col == Other#position.col orelse
	 This#position.square == Other#position.square).

%% Returns NewPosition with Digit removed from the Possible set.
%%
not_possible(This, Digit) when ?is_position(This) ->
    case This#position.possible of
	undefined ->
	    This;
	Possible ->
	    This#position{possible = possible:remove(Possible, Digit)}
    end.

to_string(This) when ?is_position(This) ->
    spud:format("~w ~w ~s", [get_number(This), get_placed(This),
			     possible:to_string(get_possible(This))]).
