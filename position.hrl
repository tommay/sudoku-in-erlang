-record(position, {number, row, col, square, possible, placed}).

-define(is_position(Term), is_record(Term, position)).

-define(position@get_number(P), P#position.number).

%% Returns true if This and Other are in the same row, column, or
%% square, else false.
%%
-define(
   position@is_excluded_by(This, Other),
       This#position.number /= Other#position.number andalso
	(This#position.row == Other#position.row orelse
	 This#position.col == Other#position.col orelse
	 This#position.square == Other#position.square)).
