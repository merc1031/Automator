-module(device_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Params), {I, {I, start_link, [Params]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Name = {name, pioneer_receiver},
    CommandMap = {command_map, #{
        "set_vol" => fun(Val) -> io_lib:format("~3..0BVL~n", [Val]) end
    }},


    Params = [Name, CommandMap],
    Receiver = ?CHILD(device, worker, Params),
    {ok, { {one_for_one, 5, 10}, [Receiver]} }.

