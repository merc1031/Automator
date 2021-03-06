-module(serial_sup).

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
    SerialTCPBridge = {serial_tcp_bridge,
                           {serial_tcp_bridge, start_link, []},
                           permanent, 5000, worker, [serial_tcp_bridge]},
    UdpBridge = {udp_bridge,
                           {udp_bridge, start_link, []},
                           permanent, 5000, worker, [udp_bridge]},
    HttpBridgeSupervisor = {http_bridge_sup,
                            {http_bridge_sup, start_link, []},
                           permanent, 5000, supervisor, [http_bridge_sup]},
    {ok, { {one_for_one, 5, 10}, [SerialTCPBridge, UdpBridge, HttpBridgeSupervisor]} }.

