-module(stats).
-behavior(gen_server).
-define(SERVER, ?MODULE).

-record(stats, {spawned = 0, solved = 0, failed = 0, current = 0, max = 0}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([spawned/0, solved/0, failed/0, get/0, to_string/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, stats}, ?MODULE, [], []).

-define(cast(Type),
	Type() ->
	       gen_server:cast(stats, Type)).

?cast(spawned).
?cast(solved).
?cast(failed).

get() ->
    gen_server:call(stats, get).

to_string(State) ->
    spud:format("spawned: ~w solved: ~w failed: ~w max: ~w",
		[State#stats.spawned, State#stats.solved, State#stats.failed,
		 State#stats.max]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #stats{}}.

handle_call(get, _From, State) ->
    {reply, State, State}.

-define(handle(Msg, Inc),
	handle_cast(Msg, State) ->
	       State2 = track(State, Inc),
	       State3 = ?increment(State2, Msg),
	       {noreply, State3}).
-define(increment(Rec, Field), Rec#stats{Field = Rec#stats.Field + 1}).

?handle(spawned, +1);
?handle(solved, -1);
?handle(failed, -1).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

track(State, Inc) ->
    Current = State#stats.current + Inc,
    case Current > State#stats.max of
	true -> State#stats{current = Current, max = Current};
	false -> State#stats{current = Current}
    end.

