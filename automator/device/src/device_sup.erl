-module(device_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, I, Type, Params), {Name, {I, start_link, [Params]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    error_logger:error_msg("~p:init()", [?MODULE]),
    Params = pioneer_receiver:get_specification(),
    Receiver = ?CHILD(pioneer_receiver, device, worker, Params),
    {ok, { {one_for_one, 5, 10}, [Receiver]} }.

