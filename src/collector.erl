-module(collector).
-include("puzzle.hrl").
-export([spawn_solver/2, solved/2, failed/1, collect_solutions/1]).

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Collector, and possibly spawns further processes
%% that also report back to the Collector.  Sends the Collector a
%% started message for its bookkeeping.
%%
spawn_solver(Collector, Func)
  when is_pid(Collector), is_function(Func) ->
    %% Need to send "started" from the current process so the
    %% Collector receives it before it receives a failed message,
    %% adjusts its count, and possibly finishes.
    started(Collector),
    limiter:run(limiter, Func).

started(Collector) when is_pid(Collector) ->
    Collector ! started.

solved(Collector, Puzzle) when is_pid(Collector), ?is_puzzle(Puzzle) ->
    Collector ! {solved, Puzzle}.

failed(Collector) when is_pid(Collector) ->
    Collector ! failed.

%% Keep track of pending results and call Yield with solutions as they
%% are found.  Loop until there are no pending results left.  Note
%% that we start with no pending results.
%%
collect_solutions(Yield)
  when is_function(Yield) ->
    collect_solutions(0, Yield).

collect_solutions(PendingCount, Yield)
  when is_integer(PendingCount), is_function(Yield) ->
    receive
	started ->
	    stats:spawned(),
	    collect_solutions(PendingCount + 1, Yield);
	{solved, Puzzle} ->
	    stats:solved(),
	    Yield(Puzzle),
	    maybe_collect_solutions(PendingCount - 1, Yield);
	failed ->
	    stats:failed(),
	    maybe_collect_solutions(PendingCount - 1, Yield);
	Msg ->
	    io:format("wtf: ~p~n", [Msg]),
	    collect_solutions(PendingCount, Yield)
    end.

maybe_collect_solutions(0, _Yield) ->
    ok;
maybe_collect_solutions(PendingCount, Yield) ->
    collect_solutions(PendingCount, Yield).
