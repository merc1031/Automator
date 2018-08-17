-module(samsung_ir).

-export([get_specification/1]).

get_specification(Conf) ->
    Name = {name, list_to_atom(maps:get(device_name, Conf))},
    CommandMap = {command_map, #{
        <<"power_on">> => ir_command(2, "171,170,21,64,21,64,21,64,22,20,21,21,21,21,21,21,21,21,21,64,21,64,21,64,21,21,22,20,21,21,21,21,21,21,21,64,21,21,21,21,21,64,22,63,21,21,21,21,21,64,21,21,21,64,21,64,22,20,21,21,21,64,21,64,21,21,21,1771,171,170,22,63,22,63,22,63,22,20,22,20,21,21,22,20,21,21,22,64,22,63,22,63,22,20,22,3700"),
        <<"power_off">> => ir_command(2, "171,170,22,63,22,63,22,63,22,20,22,21,22,20,22,20,22,21,22,63,22,63,22,63,22,21,22,21,22,21,22,21,22,21,22,21,22,21,22,21,22,63,22,63,22,21,22,21,22,63,22,63,22,63,22,63,22,21,22,20,22,63,22,63,22,21,22,1775,171,170,22,63,22,63,22,3700")
    }},

    ResponseParser = {response_parser, {
                        fun(Response, OldData, Parser) ->
                            {Working, Buffer} = device_utils:line_protocol_helper(Response, OldData, <<"\r">>),
                            Matches = device_utils:run_regex(Working, Parser),
                            {Matches, Buffer}
                        end,
                        fun() -> {ok, Re} = re:compile("completeir,1:([123]),([0-9]+)"), Re end()
    }},

    InitialDataState = {initial_data_state, #{
    }},

    #{ target := #{ ip := {Ip, Port}, type := Type } } = Conf,

    Target = {target, {Type, register_device, [Ip, Port]}},
    Params = [Name, CommandMap, Target, ResponseParser, InitialDataState],
    Params.

ir_command(Port, Pulse) ->
    io_lib:format("sendir,1:~1..0B,1,38000,1,1,~s\r", [Port, Pulse]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-endif.
