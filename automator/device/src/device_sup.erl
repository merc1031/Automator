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

    {ok, Devices} = application:get_env(ext_devices, devices),

    error_logger:error_msg("Devices ~p", [Devices]),
    Children = lists:map(
     fun({DeviceName, DeviceConf}) ->
             #{ target := _, device_type := Type } = DeviceConf,
             ?CHILD(DeviceName, device, worker, Type:get_specification(DeviceConf))
     end, Devices),

    {ok, { {one_for_one, 5, 10}, Children} }.

