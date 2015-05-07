-module(stats).
-behavior(gen_server).

-export([start/0, spawned/0, solved/0, failed/0, get/0, to_string/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(stats, {spawned = 0, solved = 0, failed = 0, current = 0, max = 0}).

-define(cast(Type),
	Type() ->
	       gen_server:cast(stats, Type)).
-define(handle(Msg, Inc),
	handle_cast(Msg, State) ->
	       State2 = track(State, Inc),
	       State3 = ?increment(State2, Msg),
	       {noreply, State3}).
-define(increment(Rec, Field), Rec#stats{Field = Rec#stats.Field + 1}).

start() ->
    gen_server:start({local, stats}, ?MODULE, [], []).

init(_Args) ->
    {ok, #stats{}}.

?cast(spawned).
?cast(solved).
?cast(failed).

get() ->
    gen_server:call(stats, get).

?handle(spawned, +1);
?handle(solved, -1);
?handle(failed, -1).

handle_call(get, _From, State) ->
    {reply, State, State}.

track(State, Inc) ->
    Current = State#stats.current + Inc,
    case Current > State#stats.max of
	true -> State#stats{current = Current, max = Current};
	false -> State#stats{current = Current}
    end.

to_string(State) ->
    spud:format("spawned: ~w solved: ~w failed: ~w max: ~w",
		[State#stats.spawned, State#stats.solved, State#stats.failed,
		 State#stats.max]).
