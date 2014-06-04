-module(device).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([translate_command/2]).

-record(device_state, {
          name :: atom,
          command_map = maps:new() :: map(),
          response_map = maps:new() :: map(),
          response_parser :: re:re(),
          target :: module(),
          listeners = lists:new() :: list(),
          data_state = maps:new() :: map(),
          clean_response_action :: fun((list()) -> list()),
          data_state_refresher = maps:new() :: map(),
          data_state_timer :: timer:tref(),
          old_data = <<>> :: binary()
}).


start_link(PropList) ->
        Name = proplists:get_value(name, PropList),
        error_logger:error_msg("~p:start_link()~n", [Name]),
        gen_server:start_link({local, Name}, ?MODULE, [PropList], []).

init([PropList]) ->
        error_logger:error_msg("~p:init()~n", [?MODULE]),
        Name = proplists:get_value(name, PropList),
        CommandMap = proplists:get_value(command_map, PropList),
        ResponseMap = proplists:get_value(response_map, PropList),
        ResponseParser = proplists:get_value(response_parser, PropList),
        Target = proplists:get_value(target, PropList, undefined),
        Listeners = proplists:get_value(listeners, PropList, []),
        InitialDataState = proplists:get_value(initial_data_state, PropList, #{}),
        CleanResponse = proplists:get_value(clean_response_action, PropList, fun(X) -> X end),
        {ok, TimerRef} = timer:send_interval(10000, refresh_data_state),
        State = #device_state{
                   name = Name,
                   command_map = CommandMap,
                   response_map = ResponseMap,
                   response_parser = ResponseParser,
                   target = Target,
                   listeners = Listeners,
                   clean_response_action = CleanResponse,
                   data_state_refresher = InitialDataState,
                   data_state_timer = TimerRef
                  },
        gen_server:cast(self(), {register, Target, InitialDataState}),
        {ok, State}.

handle_call({translate, Command}, _From, State=#device_state{target=Target,
                                                             response_parser=Parser,
                                                             command_map=CommandMap,
                                                             data_state=DataState,
                                                             clean_response_action=CleanResponseAction,
                                                             old_data=OldData,
                                                             response_map=ResponseMap}) ->
        case translate(Command, CommandMap, DataState) of
            {multi, Series} ->
                Results = lists:reverse(lists:foldl(fun({sleep, Time}=_Action, Acc) ->
                                    timer:sleep(Time),
                                    Acc;
                                 (Action, Acc) ->
                                    Result = send_command_sync(Action, Target, CleanResponseAction),
                                    [Result | Acc]
                            end, [],Series)),

                {ParsedResponses, Buffer} = parse_response(device_sup:binary_join(Results, <<"\r\n">>), OldData, Parser),
                Translated = translate(ParsedResponses, ResponseMap, DataState),

                NewState = apply_data_state(ParsedResponses, State#device_state{old_data=Buffer}),
                {reply, Translated, NewState};
            TranslatedCommand ->
                Result = send_command_sync(TranslatedCommand, Target, CleanResponseAction),
                {ParsedResponses, Buffer} = parse_response(Result, OldData, Parser),
                Translated = translate(ParsedResponses, ResponseMap, DataState),

                NewState = apply_data_state(ParsedResponses, State#device_state{old_data=Buffer}),
                {reply, Translated, NewState}
        end;
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register, undefined, _}, State) ->
        error_logger:warning_msg("Tried to register undefined target"),
        {noreply, State};
handle_cast({register, {tcp_serial, Ip, Port}}, State) ->
        serial_tcp_bridge:register_device(Ip, Port),
        gen_server:cast(self(), {init}),
        {noreply, State};
handle_cast({init}, State=#device_state{}) ->
    NewState = refresh_data_state(State),
    error_logger:error_msg("Initial state set to ~p~n", [NewState]),
    {noreply, NewState};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(refresh_data_state, State=#device_state{}) ->
    NewState = refresh_data_state(State),
    {noreply, NewState};
handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

refresh_data_state(State=#device_state{command_map=CommandMap,
                                        target=Target,
                                        clean_response_action=CleanResponseAction,
                                        data_state_refresher=DataStateRefresher,
                                        response_parser=Parser,
                                        old_data=OldData
                                      }) ->
    maps:fold(fun(_Key, InitState, StateAcc=#device_state{data_state=DataStateIn}) ->
        Cmd = proplists:get_value(cmd, InitState),
        case maps:find(Cmd, CommandMap) of
            {ok, RawCmd} ->
                Res = proplists:get_value(res, InitState),
                Result = send_command_sync(RawCmd, Target, CleanResponseAction),
                {Parsed, Rem} = parse_response(Result, OldData, Parser),
                StateAcc2 = StateAcc#device_state{old_data=Rem},
                case lists:filter(fun({L,_}) -> L =:= Res end, Parsed) of
                    [] ->
                        StateAcc2;
                    Pruned ->
                        {_, Val} = hd(lists:reverse(Pruned)), %%TODO better metric for keeping init value
                        StateAcc2#device_state{data_state=maps:put(Res, Val, DataStateIn)}
                end;
            error ->
                StateAcc
        end end, State, DataStateRefresher).
%%Optimize this to only perform the LAST update to each Resp type we care about
apply_data_state([], State=#device_state{}) ->
    State;
apply_data_state(_TranslatedResponses=[{Resp, Val}|Rest], State=#device_state{data_state=DataState}) ->
    NewDataState = case maps:find(Resp, DataState) of 
        error ->
            DataState;
        {ok, _} ->
            error_logger:error_msg("Updateing data state for ~p to ~p", [Resp, Val]),
            maps:put(Resp, Val, DataState)
    end,
    apply_data_state(Rest, State#device_state{data_state=NewDataState}).

parse_response(Response, OldData, _Parser={Parser, PState}) ->
    {Matches, Buffer} = Parser(Response, OldData, PState),
    ToTuple = fun
                  ([E,E2|_]) -> {E, E2};
                  ([E|_]) -> {E, <<"">>}
              end,
    {lists:map(ToTuple, Matches), Buffer}.

translate(Data, TranslateMap, DataState) when is_list(Data) ->
    string:join(lists:map(fun(X) -> translate(X, TranslateMap, DataState) end, Data), "");
translate(_Data={LeftRaw, RightRaw}, TranslateMap, DataState) ->
    Left = LeftRaw,
    Right = RightRaw,
    case maps:find(Left, TranslateMap) of
        {ok, Translator} when is_function(Translator, 3)->
            Translator(Left, Right, DataState);
        {ok, Translator} when is_function(Translator, 2)->
            Translator(Left, Right);
        {ok, {format, Type, Translator}} ->
            case Type of
                cmd ->
                    io_lib:format(Translator, [Left]);
                val ->
                    io_lib:format(Translator, [Right]);
                cmd_val ->
                    io_lib:format(Translator, [Left, Right]);
                val_cmd ->
                    io_lib:format(Translator, [Right, Left])
            end;
        {ok, Translator={multi, _}} ->
            Translator;
        {ok, Translator} ->
            Translator;
        error ->
            error_logger:error_msg("No translation for pattern ~p:Val ~p In ~p~n", [Left, Right, TranslateMap]),
            ""
    end.

send_command_sync(Command, {tcp_serial, Ip, Port}, CleanResponseAction) ->
    CleanResponseAction(serial_tcp_bridge:sync_serial_command({Ip, Port}, Command)).

translate_command(Device, Command) ->
    gen_server:call(Device, {translate, Command}).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

translate_datum_test() ->
    DataState = #{},
    TranslateMap = #{ 
      "set_volume" => fun(Cmd, Val) -> io_lib:format("~s ~s", [Cmd, Val]) end,
      "power_on" => "PowerOn"
    },

    "set_volume 100" = lists:flatten(translate({"set_volume", "100"}, TranslateMap, DataState)),
    "PowerOn" = lists:flatten(translate({"power_on", ""}, TranslateMap, DataState)),
    "" = translate({"another", "cmd"}, TranslateMap, DataState).

translate_with_prev_cache_state_test() ->
    DataState = #{ "VOLUME" => "10", "POWER" => "1" },
    TranslateMap = #{ "set_volume" => fun(Cmd, Val, IDataState) -> io_lib:format("~s ~s ~s", [Cmd, Val, maps:get("VOLUME", IDataState)]) end },

    "set_volume 100 10" = lists:flatten(translate({"set_volume", "100"}, TranslateMap, DataState)).

translate_multipart_test() ->
    DataState = #{},
    TranslateMap = #{ "do_seq" => {multi, ["Cmd1", "Cmd2", "Cmd3"]} },

    {multi, ["Cmd1", "Cmd2", "Cmd3"]} = translate({"do_seq", ""}, TranslateMap, DataState).
-endif.


