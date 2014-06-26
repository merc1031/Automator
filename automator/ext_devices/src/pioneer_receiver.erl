-module(pioneer_receiver).

-export([get_specification/1]).

get_specification(Conf) ->
    Name = {name, list_to_atom(maps:get(device_name, Conf))},
    CommandMap = {command_map, #{
        <<"set_vol">> => fun(_Cmd, Val) -> io_lib:format("~3..0BVL\r", [binary_to_integer(Val)]) end,
        <<"vol">> => "?V\r",
        <<"dec_vol">> => "VD\r",
        <<"inc_vol">> => "VU\r",
        <<"power_off">> => {multi, ["PF\r", {sleep, 200}, "\r"]},
        <<"power_on">> => {multi, ["\r", {sleep, 200}, "PO\r", {sleep, 2200}, "?P\r"]},
        <<"power">> => "?P\r",
        <<"input">> => "?F\r",
        <<"set_input">> => fun(_Cmd, Val) ->
                                   io_lib:format("~2..0BF\r", [to_receiver_inputs(Val)])
                           end,
        <<"mute">> => "?M\r",
        <<"mute_on">> => "MO\r",
        <<"mute_off">> => "MF\r",
        <<"mute_toggle">> => fun(_Cmd, _Val, DataState=#{}) ->
                                device_utils:with_cached_value(<<"MUT">>, DataState, fun(OldMute) ->
                                        NewMute = not either({<<"0">>, true}, {<<"1">>, false}, OldMute), %0 or 1   0 is ON
                                        io_lib:format("M~s\r", [to_receiver_on_off(NewMute)])
                                                         end)
                        end,
        <<"inc_vol_db">> => fun(_Cmd,Val, DataState=#{}) ->
                                device_utils:with_cached_value(<<"VOL">>, DataState, fun(OldVol) ->
                                        OldVolDb = (binary_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                        NewVolDb = OldVolDb + binary_to_integer(Val), %%Add Db increment
                                        NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                        io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                                                         end)
                        end,
        <<"dec_vol_db">> => fun(_Cmd,Val, DataState=#{}) ->
                                device_utils:with_cached_value(<<"VOL">>, DataState, fun(OldVol) ->
                                        OldVolDb = (binary_to_integer(OldVol) / 2) - 80.5, %%Convert to Db
                                        NewVolDb = OldVolDb - binary_to_integer(Val), %%Add Db increment
                                        NewVol = (NewVolDb + 80.5) * 2, %% Convert back to raw
                                        io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                                                         end)
                        end,
        <<"set_vol_db">> => fun(_Cmd,Val)-> 
                                NewVol = (binary_to_integer(Val) + 80.5) * 2, %% Convert back to raw
                                io_lib:format("~3..0BVL\r", [trunc(NewVol)])
                        end
    }},

    ResponseParser = {response_parser, {
                        fun(Response, OldData, Parser) ->
                            {Working, Buffer} = device_utils:line_protocol_helper(Response, OldData, <<"\r\n">>),
                            Matches = device_utils:run_regex(Working, Parser),
                            {Matches, Buffer}
                        end,
                        fun() -> {ok, Re} = re:compile("([^0-9]+)([0-9]*)"), Re end()
    }},
    ResponseMap = {response_map, #{
        <<"VOL">> => fun
                        (_, <<>>) -> "";
                        (_Cmd, Val) -> io_lib:format("vol~p~n", [(binary_to_integer(Val) / 2) - 80.5]) end,
        <<"PWR">> => fun
                        (_, <<>>) -> "";
                        (_Cmd, Val) -> io_lib:format("power~s~n", [receiver_boolean(Val)]) end,
        <<"FN">> => fun
                        (_, <<>>) -> "";
                        (_Cmd, Val) -> io_lib:format("input~s~n", [receiver_inputs(Val)]) end,
        <<"MUT">> => fun
                        (_, <<>>) -> "";
                        (_Cmd, Val) -> io_lib:format("mute~s~n", [receiver_boolean(Val)]) end
    }},

    InitialDataState = {initial_data_state, #{
        "vol" => [{cmd, <<"vol">>}, {res, <<"VOL">>}],
        "input" => [{cmd, <<"input">>}, {res, <<"FN">>}],
        "mute" => [{cmd, <<"mute">>}, {res, <<"MUT">>}]
    }},

    #{ target := #{ ip := {Ip, Port}, type := Type } } = Conf,

    Target = {target, {Type, register_device, [Ip, Port]}},
    CleanResponse = {clean_response_action,
                     fun(Resp) ->
                             %% If we clean the string of non newline, carraige return and printing characters, and its empty ...
                             case << <<Char>> || <<Char>> <= Resp, (Char == 10) or (Char == 13) or (Char >= 31), (Char =< 127) >> of
                                 <<>> -> <<"NULL0\r\n">>;
                                 R -> R
                             end
                     end},
    Params = [Name, CommandMap, ResponseParser, ResponseMap, Target, InitialDataState, CleanResponse],
    Params.

to_receiver_on_off(Val) ->
    either({true, "O"}, {false, "F"}, Val).

receiver_boolean(Val) ->
    either({<<"1">>, "Off"}, {<<"0">>, "On"}, Val).

either(TL, TR, Val) ->
    case {TL, TR} of
        { {Val, Cap}, _ } ->
            Cap;
        { _, {Val, Cap} } ->
            Cap
    end.

to_receiver_inputs(Val) ->
    case Val of
        <<"hdmi1">> ->
            19;
        <<"cd">> ->
            01
    end.

receiver_inputs(Val) ->
    case Val of
        <<"19">> ->
            "HDMI1";
        <<"20">> ->
            "HDMI2";
        <<"21">> ->
            "HDMI3";
        <<"25">> ->
            "BD";
        <<"01">> ->
            "CD";
        _ ->
            "Nothing"
    end.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
dummy_conf() ->
    #{ device_type => ?MODULE, target => #{ ip => {"1.2.3.4", 1}, type => atom }}.

can_parse_no_value_response_test() ->
    Response = <<"R\r\n">>,

    Spec = get_specification(dummy_conf()),
    {Parser, ParserState} = proplists:get_value(response_parser, Spec),
    {Matches, <<>>} = Parser(Response, <<>>, ParserState),
    [[<<"R">>, <<>>]] = Matches.

can_parse_value_response_test() ->
    Response = <<"VOL113\r\n">>,

    Spec = get_specification(dummy_conf()),
    {Parser, ParserState} = proplists:get_value(response_parser, Spec),
    {Matches, <<>>} = Parser(Response, <<>>, ParserState),
    [[<<"VOL">>, <<"113">>]] = Matches.

can_parse_multiple_value_response_test() ->
    Response = <<"VOL113\r\nFN19\r\nLM000\r\n">>,

    Spec = get_specification(dummy_conf()),
    {Parser, ParserState} = proplists:get_value(response_parser, Spec),
    {Matches, <<>>} = Parser(Response, <<>>, ParserState),
    [[<<"VOL">>, <<"113">>], [<<"FN">>, <<"19">>], [<<"LM">>, <<"000">>]] = Matches.

can_parse_value_response_from_old_data_with_buffer_test() ->
    Response = <<"113\r\nFN19">>,
    OldData = <<"VOL">>,

    Spec = get_specification(dummy_conf()),
    {Parser, ParserState} = proplists:get_value(response_parser, Spec),
    {Matches, <<"FN19">>} = Parser(Response, OldData, ParserState),
    [[<<"VOL">>, <<"113">>]] = Matches.

can_parse_no_value_response_followed_by_normal_response_test() ->
    Response = <<"R\r\nFN19\r\n">>,

    Spec = get_specification(dummy_conf()),
    {Parser, ParserState} = proplists:get_value(response_parser, Spec),
    {Matches, <<>>} = Parser(Response, <<>>, ParserState),
    [[<<"R">>, <<>>],[<<"FN">>, <<"19">>]] = Matches.

-endif.
