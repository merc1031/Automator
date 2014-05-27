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
    Name = {name, pioneer_receiver},
    CommandMap = {command_map, #{
        "set_vol" => fun(_Cmd, Val) -> io_lib:format("~3..0BVL\r", [list_to_integer(Val)]) end,
        "vol" => "?V\r",
        "dec_vol" => "VD\r",
        "inc_vol" => "VU\r"
    }},

    ResponseParser = {response_parser, fun() -> {ok, Re} = re:compile("([^0-9]+?)([0-9]+?)\r\n"), Re end() },
    ResponseMap = {response_map, #{
        "VOL" => fun(_Cmd, Val) -> io_lib:format("vol~p~n", [(list_to_integer(Val) / 2) - 80.5]) end
    }},

    InitialDataState = {initial_data_state, #{
        "vol" => [{cmd, "vol"}, {res, "VOL"}]
    }},
    Target = {target, {tcp_serial, "192.168.1.124", 4999}},
    Params = [Name, CommandMap, ResponseParser, ResponseMap, Target, InitialDataState],
    Receiver = ?CHILD(pioneer_receiver, device, worker, Params),
    {ok, { {one_for_one, 5, 10}, [Receiver]} }.

