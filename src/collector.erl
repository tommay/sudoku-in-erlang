-module(collector).
-include("puzzle.hrl").
-export([spawn_solver/2, yield/2, collect_and_yield_results/1]).

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

yield(Collector, Result) when is_pid(Collector) ->
    Collector ! {result, Result}.

%% Keep track of pending results and call Yield with solutions as they
%% are found.  Loop until there are no pending results left.  Note
%% that we start with no pending results.
%%
collect_and_yield_results(Yield)
  when is_function(Yield) ->
    collect_and_yield_results(0, Yield).

collect_and_yield_results(PendingCount, Yield)
  when is_integer(PendingCount), is_function(Yield) ->
    receive
	started ->
	    collect_and_yield_results(PendingCount + 1, Yield);
	{result, Result} ->
	    Yield(Result),
	    case PendingCount - 1 of
		0 -> ok;
		N -> collect_and_yield_results(N, Yield)
	    end;
	Msg ->
	    io:format("wtf: ~p~n", [Msg]),
	    collect_and_yield_results(PendingCount, Yield)
    end.
