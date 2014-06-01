-module(device_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([line_protocol_helper/3]).

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
        "power" => "?P\r",
        "input" => "?F\r",
        "inc_vol_db" => fun(_Cmd,Val, DataState=#{}) ->
                                with_cached_value("VOL", DataState, fun(OldVol) ->
                                        OldVolDb = (list_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                        NewVolDb = OldVolDb + list_to_integer(Val), %%Add Db increment
                                        NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                        io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                                                         end)
                        end,
        "dec_vol_db" => fun(_Cmd,Val, DataState=#{}) ->
                                with_cached_value("VOL", DataState, fun(OldVol) ->
                                        OldVolDb = (list_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                        NewVolDb = OldVolDb - list_to_integer(Val), %%Add Db increment
                                        NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                        io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                                                         end)
                        end,
        "set_vol_db" => fun(_Cmd,Val)-> 
                                NewVol = (list_to_integer(Val) + 80.5) * 2, %% Convert back to raw
                                io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                        end
    }},

    ResponseParser = {response_parser, {
                        fun(Response, OldData, Parser) ->
                            {Working, Buffer} = line_protocol_helper(Response, OldData, <<"\r\n">>),
                            {match, Matches} = re:run(Working, Parser, [global, {capture, all_but_first, list}]),
                            {Matches, Buffer}
                        end,
                        fun() -> {ok, Re} = re:compile("([^0-9]+?)([0-9]*?)"), Re end()
    }},
    ResponseMap = {response_map, #{
        "VOL" => fun(_Cmd, Val) -> io_lib:format("vol~p~n", [(list_to_integer(Val) / 2) - 80.5]) end,
        "PWR" => fun(_Cmd, Val) -> io_lib:format("power~s~n", [case Val of "1" -> "Off"; "0" -> "On" end]) end,
        "FN" => fun(_Cmd, Val) -> io_lib:format("input~s~n", [receiver_inputs(Val)]) end
    }},

    InitialDataState = {initial_data_state, #{
        "vol" => [{cmd, "vol"}, {res, "VOL"}],
        "input" => [{cmd, "input"}, {res, "FN"}]
    }},
    Target = {target, {tcp_serial, "192.168.1.124", 4999}},
    CleanResponse = {clean_response_action, fun(Resp) -> case lists:filter(fun(Char) -> (Char >= 10) and (Char =< 126) end, Resp) of
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

%-spec line_protocol_helper(binary(), binary(), binary()) -> {binary(), binary()}.
-spec line_protocol_helper(A, A, A) -> {A, A} when A :: binary().
line_protocol_helper(NewData, OldData, Delim) ->
    Total = <<OldData/binary, NewData/binary>>,
    Acc = fun AccumDataAndParse(Remaining, {Ret, <<>>}) ->
        case binary:split(Remaining, Delim) of
            [Data] -> % no delim found
                {Ret, Data};
            [Line, Rest] ->
                AccumDataAndParse(Rest, {<<Ret/binary, Line/binary>>, <<>>})
        end
    end,
    Acc(Total, {<<>>,<<>>}).

with_cached_value(Key, DataState, Operation) ->
    case maps:find(Key, DataState) of %%Lookup Raw Stored
        {ok, CachedVal} ->
            Operation(CachedVal);
        error ->
            ""
    end.


