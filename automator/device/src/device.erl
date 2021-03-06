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
-export([query_timeout/2]).
-export([query_should_wait/2]).

-record(device_state, {
          name :: atom,
          command_map = maps:new() :: map(),
          response_map = maps:new() :: map(),
          response_parser :: re:re(),
          target :: mfa(),
          target_locator :: map(),
          listeners = [] :: list(),
          data_state = maps:new() :: map(),
          clean_response_action :: fun((list()) -> list()),
          data_state_refresher = maps:new() :: map(),
          waiting = sets:new() :: sets:set(),
          data_state_timer :: timer:tref(),
          old_data = <<>> :: binary()
}).


start_link(PropList) ->
        Name = proplists:get_value(name, PropList),
        error_logger:info_msg("~p:start_link() for ~p called from ~p", [?MODULE, Name, self()]),
        gen_server:start_link({local, Name}, ?MODULE, [PropList], []).

init([PropList]) ->
        process_flag(trap_exit, true),
        error_logger:info_msg("~p ~p: ~p:init()", [?MODULE, self(), ?MODULE]),
        Name = proplists:get_value(name, PropList),
        CommandMap = proplists:get_value(command_map, PropList),
        ResponseMap = proplists:get_value(response_map, PropList, #{}),
        % TODO undefined is bad here. should have a default noop parser
        ResponseParser = proplists:get_value(response_parser, PropList, undefined),
        Target = proplists:get_value(target, PropList, undefined),
        Listeners = proplists:get_value(listeners, PropList, []),
        InitialDataState = proplists:get_value(initial_data_state, PropList, #{}),
        CleanResponse = proplists:get_value(clean_response_action, PropList, fun(X) -> X end),
        {ok, TimerRef} = timer:send_interval(10000, refresh_data_state),
        DataState = maps:fold(fun(_,[_, {res, V}], Acc) -> maps:put(V, <<"">>, Acc) end, #{}, InitialDataState),
        State = #device_state{
                   name = Name,
                   command_map = CommandMap,
                   response_map = ResponseMap,
                   response_parser = ResponseParser,
                   target = Target,
                   listeners = Listeners,
                   clean_response_action = CleanResponse,
                   data_state_refresher = InitialDataState,
                   data_state_timer = TimerRef,
                   data_state = DataState
                  },
        gen_server:cast(self(), {register, Target}),
        {ok, State}.

handle_call({query_timeout, CmdName}, _From, State=#device_state{command_map=CommandMap}) ->
    Reply = case maps:find(CmdName, CommandMap) of
                {ok, {multi, Series}} ->
                    lists:foldl(
                      fun({sleep, Time}=_, Acc) ->
                              Acc + Time;
                         (_, Acc) ->
                              Acc + 250
                      end, 0, Series);

                {ok, _} ->
                    250;
                _ ->
                    0
    end,
    {reply, Reply, State};
handle_call({query_should_wait, CmdName}, _From, State=#device_state{
                                                          name=Name
                                                         }) ->
    Reply = case erlang:function_exported(Name, should_wait, 1) of
        true ->
            Name:should_wait(CmdName);
        false ->
            yes
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register, undefined, _}, State) ->
        error_logger:warning_msg("~p ~p: Tried to register with no target defined!", [?MODULE, self()]),
        {noreply, State};
handle_cast({register, {Module, Function, Args}}, State=#device_state{name=Name}) ->
    ok = erlang:apply(Module, Function, [self() | [Name | Args]]),
    State2 = receive
        {registered, TargetName} ->
            State#device_state{target_locator = #{module=>Module, name=>TargetName}}
    after 2000 ->
            throw({"Failed to register target"})
    end,
    gen_server:cast(self(), init),
    {noreply, State2};
handle_cast(init, State=#device_state{}) ->
    NewState = refresh_data_state(State),
    error_logger:info_msg("~p ~p: ~p Initial state set to ~p", [?MODULE, self(), State#device_state.name, NewState]),
    {noreply, NewState};
handle_cast({translate, Listener, Command}, State=#device_state{target_locator=Target,
                                                             command_map=CommandMap,
                                                             waiting=Waiting,
                                                             data_state=DataState
                                                             }) ->
        State2 = State#device_state{waiting=sets:add_element({Listener, link(Listener)}, Waiting)}, %% Todo change to monitor and 'DOWN'
        error_logger:info_msg("~p ~p: Device has queued a listener ~p in ~p", [?MODULE, self(), Listener, Waiting]),
        error_logger:info_msg("~p ~p: Device has received a command ~p", [?MODULE, self(), Command]),
        case translate(Command, CommandMap, DataState) of
            {multi, Series} ->
                lists:foreach(fun({sleep, Time}=_Action) ->
                                    timer:sleep(Time);
                                 (Action) ->
                                    send_command(Action, Target)
                            end, Series);

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
                                           response_parser=Parser,
                                           old_data=OldData
                                           }) ->
    error_logger:info_msg("~p ~p: Device (~p) has received a response ~p", [?MODULE, self(), State#device_state.name, Response]),
    {ParsedResponses, Buffer} = parse_response(CleanResponseAction(Response), OldData, Parser),
    State2 = State#device_state{old_data=Buffer},
    Translated = translate(ParsedResponses, ResponseMap, DataState),

    reply(ParsedResponses, Translated, State2),

    NewState = apply_data_state(ParsedResponses, State2),
    {noreply, NewState};
handle_info(_Packet={'EXIT', Pid, Reason}, State=#device_state{waiting=Waiting}) ->
    error_logger:info_msg("~p ~p: A waiting listener died ~p from ~p because ~p", [?MODULE, self(), Pid, Waiting, Reason]),
    {noreply, State#device_state{waiting=sets:filter(fun({ListPid, _ListLinkResult}) -> Pid =/= ListPid end, Waiting)}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

populate_reply(ParsedResponses, ResponseMap, DataState) ->
    Keys = lists:map(fun({L,_}) -> L end, ParsedResponses),
    Unnaccounted = maps:without(Keys, DataState),
    lists:flatten(maps:fold(fun(K,V,Acc) -> [Acc, translate({K,V}, ResponseMap, DataState)] end, [], Unnaccounted)).

reply(ParsedResponses, Translated, #device_state{
                           waiting=Waiting,
                           data_state=DataState,
                           response_map=ResponseMap
                          }) ->
    AdditionalTranslated = populate_reply(ParsedResponses, ResponseMap, DataState),
    FinalTranslated = lists:flatten([Translated, AdditionalTranslated]),
    lists:foreach(fun({Listener, _}) ->
                          error_logger:info_msg("~p ~p: Replying to ~p with ~p", [?MODULE, self(), Listener, FinalTranslated]),
                          Listener ! {response, FinalTranslated} end, sets:to_list(Waiting)).

refresh_data_state(State=#device_state{command_map=CommandMap,
                                        target_locator=Target,
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

%%Optimize this to only perform the LAST update to each Resp type we care about
apply_data_state([], State=#device_state{}) ->
    State;
apply_data_state(_TranslatedResponses=[{Resp, Val}|Rest], State=#device_state{data_state=DataState}) ->
    NewDataState = case maps:find(Resp, DataState) of 
        error ->
            DataState;
        {ok, _} ->
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
            error_logger:error_msg("~p ~p: had no translation for pattern ~p(Val ~p) In ~p", [?MODULE, self(), Left, Right, TranslateMap]),
            ""
    end.

send_command(Command, #{module:=Module, name:=Target}) ->
    Module:send_command(Target, Command).

translate_command(Listener, Device, Command) ->
    gen_server:cast(Device, {translate, Listener, Command}).

query_timeout(Device, CmdName) ->
    gen_server:call(Device, {query_timeout, CmdName}).

query_should_wait(Device, CmdName) ->
    gen_server:call(Device, {query_should_wait, CmdName}).

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


