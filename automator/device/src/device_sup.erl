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
        "inc_vol" => "VU\r",
        "power_off" => {multi, ["PF\r", {sleep, 200}, "\r"]},
        "power_on" => {multi, ["\r", {sleep, 200}, "PO\r", {sleep, 2200}, "?P\r"]},
        "inc_vol_db" => fun(_Cmd,Val, DataState=#{}) -> 
                                OldVol = maps:get("VOL", DataState), %%Lookup Raw Stored
                                OldVolDb = (list_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                NewVolDb = OldVolDb + list_to_integer(Val), %%Add Db increment
                                NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                        end,
        "dec_vol_db" => fun(_Cmd,Val, DataState=#{}) -> 
                                OldVol = maps:get("VOL", DataState), %%Lookup Raw Stored
                                OldVolDb = (list_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                NewVolDb = OldVolDb - list_to_integer(Val), %%Add Db increment
                                NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                        end
    }},

    ResponseParser = {response_parser, fun() -> {ok, Re} = re:compile("([^0-9]+?)([0-9]*?)\r\n"), Re end() },
    ResponseMap = {response_map, #{
        "VOL" => fun(_Cmd, Val) -> io_lib:format("vol~p~n", [(list_to_integer(Val) / 2) - 80.5]) end,
        "PWR" => fun(_Cmd, Val) -> io_lib:format("power~p~n", [case Val of "1" -> "Off"; "0" -> "On" end]) end,
        "FN" => fun(_Cmd, Val) -> io_lib:format("input~p~n", [receiver_inputs(Val)]) end
    }},

    InitialDataState = {initial_data_state, #{
        "vol" => [{cmd, "vol"}, {res, "VOL"}]
    }},
    Target = {target, {tcp_serial, "192.168.1.124", 4999}},
    CleanResponse = {clean_response_action, fun(Resp) -> case lists:filter(fun(Char) -> Char =/= 0 end, Resp) of
                                     [] -> "NULL0\r\n";
                                     R -> R
                                 end 
                    end},
    Params = [Name, CommandMap, ResponseParser, ResponseMap, Target, InitialDataState, CleanResponse],
    Receiver = ?CHILD(pioneer_receiver, device, worker, Params),
    {ok, { {one_for_one, 5, 10}, [Receiver]} }.

receiver_inputs(Val) ->
    case Val of
        "19" ->
            "HDMI1";
        "20" ->
            "HDMI2";
        "21" ->
            "HDMI3";
        "25" ->
            "BD";
        "01" ->
            "CD";
        _ ->
            "Nothing"
    end.

