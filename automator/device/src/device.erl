-module(device).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([translate_command/3]).

-record(device_state, {
          name :: atom,
          command_map = maps:new() :: map(),
          response_map = maps:new() :: map(),
          response_parser :: re:re(),
          target :: module(),
          listeners = [] :: list(),
          data_state = maps:new() :: map(),
          clean_response_action :: fun((list()) -> list()),
          data_state_refresher = maps:new() :: map(),
          waiting = [] :: list(),
          data_state_timer :: timer:tref()
}).


start_link(PropList) ->
        Name = proplists:get_value(name, PropList),
        error_logger:error_msg("~p:start_link()~n", [Name]),
        gen_server:start_link({local, Name}, ?MODULE, [PropList], []).

init([PropList]) ->
        process_flag(trap_exit, true),
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
        gen_server:cast(self(), {register, Target}),
        {ok, State}.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register, undefined, _}, State) ->
        error_logger:warning_msg("Tried to register undefined target"),
        {noreply, State};
handle_cast({register, {tcp_serial, Ip, Port}}, State=#device_state{name=Name}) ->
        serial_tcp_bridge:register_device(Name, Ip, Port),
        gen_server:cast(self(), init),
        {noreply, State};
handle_cast(init, State=#device_state{}) ->
    NewState = refresh_data_state(State),
    error_logger:error_msg("Initial state set to ~p~n", [NewState]),
    {noreply, NewState};
handle_cast({translate, Listener, Command}, State=#device_state{target=Target,
                                                             command_map=CommandMap,
                                                             waiting=Waiting,
                                                             data_state=DataState
                                                             }) ->
        State2 = State#device_state{waiting=lists:append({Listener, link(Listener)}, Waiting)},
        case translate(Command, CommandMap, DataState) of
            {multi, Series} ->
                lists:reverse(lists:foreach(fun({sleep, Time}=_Action) ->
                                    timer:sleep(Time);
                                 (Action) ->
                                    send_command(Action, Target)
                            end, Series));

            TranslatedCommand ->
                send_command(TranslatedCommand, Target)
        end,
        {noreply, State2};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(refresh_data_state, State=#device_state{}) ->
    NewState = refresh_data_state(State),
    {noreply, NewState};
handle_info({response, Response}, State=#device_state{
                                           clean_response_action=CleanResponseAction,
                                           response_map=ResponseMap,
                                           data_state=DataState,
                                           waiting=Waiting,
                                           response_parser=Parser
                                           }) ->
    ParsedResponses = parse_response(lists:flatten(CleanResponseAction(Response)), Parser),
    Translated = translate(ParsedResponses, ResponseMap, DataState),

    lists:foreach(fun(Listener) -> Listener ! {response, Translated} end, Waiting),

    NewState = apply_data_state(ParsedResponses, State),
    {noreply, NewState};
handle_info(_Packet={'EXIT', Pid, _Reason}, State=#device_state{waiting=Waiting}) ->
    {noreply, State#device_state{waiting=lists:filter(fun({ListPid, _ListLinkResult}) -> Pid =/= ListPid end, Waiting)}};
handle_info(_Info, State) ->
        {noreply, State}.


%                ParsedResponses = parse_response(lists:flatten(Results), Parser),
%                Translated = translate(ParsedResponses, ResponseMap, DataState),
%
%                NewState = apply_data_state(ParsedResponses, State),
%                {reply, Translated, NewState};
%
%                ParsedResponses = parse_response(Result, Parser),
%                Translated = translate(ParsedResponses, ResponseMap, DataState),
%
%                NewState = apply_data_state(ParsedResponses, State),
%
terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

refresh_data_state(State=#device_state{command_map=CommandMap,
                                        target=Target,
                                        data_state_refresher=DataStateRefresher
                                        }) ->
    maps:map(fun(_Key, InitState) ->
        Cmd = proplists:get_value(cmd, InitState),
        case maps:find(Cmd, CommandMap) of
            {ok, RawCmd} ->
                send_command(RawCmd, Target);
            error ->
                ok
        end end, DataStateRefresher),
    State.

%                Res = proplists:get_value(res, InitState),
%                Parsed = parse_response(Result, Parser),
%                case lists:filter(fun({L,_}) -> L =:= Res end, Parsed) of
%                    [] ->
%                        StateAcc;
%                    Pruned ->
%                        {_, Val} = hd(lists:reverse(Pruned)), %%TODO better metric for keeping init value
%                        StateAcc#device_state{data_state=maps:put(Res, Val, DataStateIn)}
%                end;
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

parse_response(Response, Parser) ->
    {match, Matches} = re:run(Response, Parser, [global, {capture, all_but_first, list}]),
    ToTuple = fun
                  ([E,E2|_]) -> {E, E2};
                  ([E|_]) -> {E, ""}
              end,
    lists:map(ToTuple, Matches).

translate(Data, TranslateMap, DataState) when is_list(Data) ->
    string:join(lists:map(fun(X) -> translate(X, TranslateMap, DataState) end, Data), "");
translate(_Data={LeftRaw, RightRaw}, TranslateMap, DataState) ->
    Left = normalize(LeftRaw),
    Right = normalize(RightRaw),
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
            error_logger:error_msg("No translation for pattern ~p:Val ~p ~n", [Left, Right]),
            ""
    end.

normalize(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
normalize(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
normalize(String) ->
    String.

send_command(Command, {tcp_serial, Ip, Port}) ->
    serial_tcp_bridge:send_command({Ip, Port}, Command).

%send_command_sync(Command, {tcp_serial, Ip, Port}, CleanResponseAction) ->
%    CleanResponseAction(serial_tcp_bridge:sync_serial_command({Ip, Port}, Command)).

translate_command(Listener, Device, Command) ->
    gen_server:cast(Device, {translate, Listener, Command}).

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


