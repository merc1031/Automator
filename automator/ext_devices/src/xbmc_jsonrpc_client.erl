-module(xbmc_jsonrpc_client).

-export([get_specification/1]).
-export([should_wait/1]).

get_specification(Conf) ->
    Name = {name, list_to_atom(maps:get(device_name, Conf))},

    CommandMap = {command_map, #{
                    <<"get_active_players">> => #{ <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"Player.GetActivePlayers">>, <<"id">> => 1}
    }},

    InitialDataState = {initial_data_state, #{
    }},

    #{ target := #{ ip := {Ip, Port}, type := Type, pass := Pass, user := User } } = Conf,

    Target = {target, {
                Type,
                register_device,
                [
                 Ip,
                 Port,
                 #{
                   req_type => post,
                   url_format => "http://~s:~p/jsonrpc",
                   headers => [{"Authorization", "Basic " ++ base64:encode_to_string(lists:flatten(io_lib:format("~s:~s", [User, Pass])))}],
                   user => User,
                   pass => Pass
                  }
                ]
               }
             },

    ResponseParser = {response_parser, {
                        fun({Response, Req}, _OldData, _State) ->
                            {_, Data} = Req,
                            Data0 = jiffy:decode(Data, [return_maps]),
                            ErlData = jiffy:decode(Response, [return_maps]),
                            #{ <<"result">> := Res } = ErlData,
                            #{ <<"method">> := Meth } = Data0,
                            {[[Meth,Res]], <<"">>}
                        end,
                        none
    }},
    ResponseMap = {response_map, #{
                     <<"Player.GetActivePlayers">> => fun
                     (_Cmd, []) -> io_lib:format("There is no active player");
                     (_Cmd, Val) ->
                                                              [#{ <<"playerid">> := Id, <<"type">> := T}| _] = Val,
                                                              io_lib:format("The active player is ~s:~p", [T, Id])
                                                      end
    }},

    Params = [Name, ResponseParser, ResponseMap, CommandMap, Target, InitialDataState],
    Params.

should_wait(_) ->
    yes.
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-endif.
