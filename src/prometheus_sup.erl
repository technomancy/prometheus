
-module(prometheus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Prometheus = {prometheus, {prometheus, start_link, []},
                  permanent, infinity, worker, [gen_server]},
    Sensor = {prometheus_sensor, {prometheus_sensor, start_link, []},
              permanent, infinity, worker, [gen_server]},
    Regulator = {prometheus_regulator, {prometheus_regulator, start_link, []},
              permanent, infinity, worker, [gen_server]},
    {ok, { {one_for_one, 5, 10}, [Prometheus, Sensor, Regulator]} }.
