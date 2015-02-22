-module(semaphore).
-export([start/2, try_acquire/1]).

-define(TRY_ACQUIRE, try_acquire).
-define(ACQUIRED, acquired).
-define(NOT_ACQUIRED, not_acquired).

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    Pid = spawn(fun () -> loop(Name, Permits) end),
    register(Name, Pid).

%% Non-blocking call to acquire a semaphore permit.  The semaphore is
%% released when the process terminates.
%%
try_acquire(Name) ->
    Name ! {self(), ?TRY_ACQUIRE},
    receive
	{Name, ?ACQUIRED} ->
	    true;
	{Name, ?NOT_ACQUIRED} ->
	    false
    end.

loop(Name, Permits) ->
    receive
	{Pid, ?TRY_ACQUIRE} ->
	    case Permits == 0 of
		true ->
		    Pid ! {Name, ?NOT_ACQUIRED},
		    loop(Name, Permits);
		false ->
		    monitor(process, Pid),
		    Pid ! {Name, ?ACQUIRED},
		    loop(Name, Permits - 1)
	    end;
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    loop(Name, Permits + 1)
    end.
