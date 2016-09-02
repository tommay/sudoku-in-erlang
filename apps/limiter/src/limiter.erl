-module(limiter).
-behaviour(gen_server).

%% This code uses an integer count of permits to limit the number of
%% spawned processes.  Spawned processes can also be limited by using
%% a process pool but this is somewhat faster and the code is simpler.

-record(state, {permits}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, run/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% Returns {ok, Pid}, or not.
%%
start_link({Name, Permits}) ->
    gen_server:start_link({local, Name}, ?MODULE, Permits, []).

%% Spawns a process to run Func if there is a permit available, else
%% runs Func in-process.
%%
run(Limiter, Func) ->
    case gen_server:call(Limiter, {run, Func}) of
	true ->
	    void;
	false ->
	    Func()
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Permits) ->
    {ok, #state{permits = Permits}}.

handle_call({run, Func}, _From, State) ->
    case State#state.permits == 0 of
	true ->
	    {reply, false, State};
	false ->
	    %% Spawn a process and monitor it.  When the process
	    %% terminates we'll get a 'DOWN' message in handle_info.
	    monitor(process, spawn(Func)),
	    {reply, true, State#state{permits = State#state.permits - 1}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State#state{permits = State#state.permits + 1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
