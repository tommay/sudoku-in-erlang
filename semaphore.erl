-module(semaphore).
-export([start/2, acquire/1]).

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    Pid = spawn(fun () -> run(Name, Permits) end),
    register(Name, Pid).

%% Blocks until the semaphore is acquired.  The semaphore is released
%% when the process terminates.
%%
acquire(Name) ->
    Name ! {self(), acquire},
    receive
	{Name, acquired} ->
	    true
    end.

run(Name, Permits) ->
    case Permits of
	0 ->
	    receive
		{'DOWN', _Ref, process, _Pid, _Reason} ->
		    run(Name, Permits + 1)
	    end;
	_ ->
	    receive
		{Pid, acquire} ->
		    monitor(process, Pid),
		    Pid ! {Name, acquired},
		    run(Name, Permits - 1);
		{'DOWN', _Ref, process, _Pid, _Reason} ->
		    run(Name, Permits + 1)
	    end
    end.
