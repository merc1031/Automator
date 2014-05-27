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
          data_state = maps:new() :: map()
}).


start_link(PropList) ->
        Name = proplists:get_value(name, PropList),
        error_logger:error_msg("~p:start_link()~n", [Name]),
        error_logger:error_msg("start_link() args ~p~n", [PropList]),
        gen_server:start_link({local, Name}, ?MODULE, [PropList], []).

init([PropList]) ->
        error_logger:error_msg("~p:init()~n", [?MODULE]),
        error_logger:error_msg("init() args ~p~n", [PropList]),
        Name = proplists:get_value(name, PropList),
        CommandMap = proplists:get_value(command_map, PropList),
        ResponseMap = proplists:get_value(response_map, PropList),
        ResponseParser = proplists:get_value(response_parser, PropList),
        Target = proplists:get_value(target, PropList, undefined),
        Listeners = proplists:get_value(listeners, PropList, []),
        InitialDataState = proplists:get_value(initial_data_state, PropList, #{}),
        State = #device_state{
                   name = Name,
                   command_map = CommandMap,
                   response_map = ResponseMap,
                   response_parser = ResponseParser,
                   target = Target,
                   listeners = Listeners
                  },
        gen_server:cast(self(), {register, Target, InitialDataState}),
        {ok, State}.

handle_call({translate, Command}, _From, State=#device_state{target=Target,
                                                             response_parser=Parser,
                                                             command_map=CommandMap,
                                                             data_state=DataState,
                                                             response_map=ResponseMap}) ->
        error_logger:error_msg("In ~p ~p", [State#device_state.name, Command]),
        case translate(Command, CommandMap, DataState) of
            {multi, Series} ->
                Results = lists:reverse(lists:foldl(fun({sleep, Time}=_Action, Acc) ->
                                    error_logger:error_msg("About to sleep ~p~n", [Time]),
                                    timer:sleep(Time),
                                    Acc;
                                 (Action, Acc) ->
                                    error_logger:error_msg("Sending ~p~n", [Action]),
                                    Result = send_command_sync(Action, Target),
                                    error_logger:error_msg("Got ~p~n", [Result]),
                                    [Result | Acc]
                            end, [],Series)),

                error_logger:error_msg("multi results is ~p~n", [Results]),
                ParsedResponses = parse_response(lists:flatten(Results), Parser),
                error_logger:error_msg("multi parsse is ~p~n", [ParsedResponses]),
                Translated = translate(ParsedResponses, ResponseMap, DataState),

                NewState = apply_data_state(ParsedResponses, State),
                {reply, Translated, NewState};
            TranslatedCommand ->
                Result = send_command_sync(TranslatedCommand, Target),
                ParsedResponses = parse_response(Result, Parser),
                Translated = translate(ParsedResponses, ResponseMap, DataState),

                NewState = apply_data_state(ParsedResponses, State),
                {reply, Translated, NewState}
        end;
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register, undefined, _}, State) ->
        error_logger:warning_msg("Tried to register undefined target"),
        {noreply, State};
handle_cast({register, {tcp_serial, Ip, Port}, InitialDataState}, State) ->
        serial_tcp_bridge:register_device(Ip, Port),
        gen_server:cast(self(), {init, InitialDataState}),
        {noreply, State};
handle_cast({init, InitialDataState}, State=#device_state{command_map=CommandMap,
                                                         target=Target,
                                                         response_parser=Parser}) ->
    NewState = maps:fold(fun(_Key, InitState, StateAcc=#device_state{data_state=DataStateIn}) ->
        Cmd = proplists:get_value(cmd, InitState),
        case maps:find(Cmd, CommandMap) of
            {ok, RawCmd} ->
                Res = proplists:get_value(res, InitState),
                Result = send_command_sync(RawCmd, Target),
                Parsed = parse_response(Result, Parser),
                case lists:filter(fun({L,_}) -> L =:= Res end, Parsed) of
                    [] ->
                        StateAcc;
                    Pruned ->
                        {_, Val} = hd(lists:reverse(Pruned)), %%TODO better metric for keeping init value
                        StateAcc#device_state{data_state=maps:put(Res, Val, DataStateIn)}
                end;
            error ->
                StateAcc
        end end, State, InitialDataState),
    error_logger:error_msg("Initial state set to ~p~n", [NewState]),
    {noreply, NewState};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

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

parse_response(Response, Parser) ->
    error_logger:error_msg("Response ~p~n", [Response]),
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

purge_null(Return) ->
    lists:filter(fun(Char) -> Char =/= 0 end, Return).

send_command_sync(Command, {tcp_serial, Ip, Port}) ->
    case purge_null(serial_tcp_bridge:sync_serial_command({Ip, Port}, Command)) of
        [] ->
            "NULL0\r\n"; %% handle null response
        [0] ->
            "NULL0\r\n"; %% handle null response
        R ->
            error_logger:error_msg("What is R ~p~n", [R]),
            R
    end.

translate_command(Device, Command) ->
    gen_server:call(Device, {translate, Command}).


