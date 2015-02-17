-module(semaphore).
-export([start/2, acquire/1]).

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Remaining) ->
    Pid = spawn(fun () -> run(Name, Remaining * 2) end),
    register(Name, Pid).

%% Blocks until the semaphore is acquire.  The semaphore is released
%% when the process terminates.
%%
acquire(Name) ->
    Name ! {self(), acquire},
    receive
	{Name, acquired} ->
	    true
    end.

run(Name, Remaining) ->
    case Remaining of
	0 ->
	    receive
		{'DOWN', _Ref, process, _Pid, _Reason} ->
		    run(Name, Remaining + 1)
	    end;
	_ ->
	    receive
		{Pid, acquire} ->
		    monitor(process, Pid),
		    Pid ! {Name, acquired},
		    run(Name, Remaining - 1);
		{'DOWN', _Ref, process, _Pid, _Reason} ->
		    run(Name, Remaining + 1)
	    end
    end.
