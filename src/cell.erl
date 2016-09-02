-module(cell).
-export([new/1, get_number/1, get_possible/1, place/2, get_placed/1]).
-export([is_excluded_by/2, not_possible/2]).
-export([to_string/1]).

%% A Cell records the state of an individual sudoku square.
%% They're not called "squares" because that's used for the squares
%% that contain nine postitions.  A Cell knows:
%% - its number from 0 to 80.  Cells are numbered across the board
%%   from left to right and top to bottom.
%% - its row.  Rows are numbered 0 to 8 from top to bottom.
%% - its column.  Rows are numbered 0 to 8 from left to right.
%% - its square.  Squares are numbered across the board
%%   from left to right and top to bottom.
%% - which digits are Possible to place in this Cell, i.e., which
%%   digits don't conflict with previously placed digits in other
%%   Cells in the same row/column/square.  If placed is set, then
%%   possible is undefined.
%% - which digit has been placed in this Cell, or undefined if no
%%   digit has been placed yet.

-record(cell, {number, row, col, square, possible, placed}).
-define(is_cell(Term), is_record(Term, cell)).

%% Returns a new Cell at position Number.  Determine the Cell's row,
%% column, and square, create a new Possible with all digits possible,
%% and set placed to undefined.
%%
new(Number) ->
    Row = Number div 9,
    Col = Number rem 9,
    Square = (Row div 3)*3 + (Col div 3),
    #cell{number = Number, row = Row, col = Col, square = Square,
	      possible = possible:new(), placed = undefined}.

get_number(This) when ?is_cell(This) ->
    This#cell.number.

get_possible(This) when ?is_cell(This) ->
    This#cell.possible.

place(This, Digit) when ?is_cell(This) ->
    This#cell{placed = Digit, possible = undefined}.

get_placed(This) when ?is_cell(This) ->
    This#cell.placed.

%% Returns true if This and Other are in the same row, column, or
%% square, else false.
%%
is_excluded_by(This, Other)
  when ?is_cell(This), ?is_cell(Other) ->
    %% Shouldn't have to check that it's the same Cell.
    get_number(This) /= get_number(Other) andalso
	(This#cell.row == Other#cell.row orelse
	 This#cell.col == Other#cell.col orelse
	 This#cell.square == Other#cell.square).

%% Returns a new Cell with Digit removed from the Possible set.
%%
not_possible(This, Digit) when ?is_cell(This) ->
    case This#cell.possible of
	undefined ->
	    This;
	Possible ->
	    This#cell{possible = possible:remove(Possible, Digit)}
    end.

to_string(This) when ?is_cell(This) ->
    spud:format("~w ~w ~s", [get_number(This), get_placed(This),
			     possible:to_string(get_possible(This))]).
