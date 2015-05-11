-module(limiter_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Permits), {I, {I, start_link, [Permits]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
    {ok,
     { {one_for_one, 5, 10}, [?CHILD(limiter, worker, StartArgs)] }
    }.
