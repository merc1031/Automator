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
          listeners = lists:new() :: list()
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
        State = #device_state{
                   name = Name,
                   command_map = CommandMap,
                   response_map = ResponseMap,
                   response_parser = ResponseParser,
                   target = Target,
                   listeners = Listeners
                  },
        gen_server:cast(self(), {register, Target}),
        {ok, State}.

handle_call({translate, Command}, _From, State=#device_state{target=Target,
                                                             response_parser=Parser,
                                                             command_map=CommandMap,
                                                             response_map=ResponseMap}) ->
        error_logger:error_msg("In ~p ~p", [State#device_state.name, Command]),
        TranslatedCommand = translate(Command, CommandMap),
        Result = send_command_sync(TranslatedCommand, Target),

        Translated = translate(parse_response(Result, Parser), ResponseMap),

        {reply, Translated, State};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register, undefined}, State) ->
        error_logger:warning_msg("Tried to register undefined target"),
        {noreply, State};
handle_cast({register, {tcp_serial, Ip, Port}}, State) ->
        serial_tcp_bridge:register_device(Ip, Port),
        {noreply, State};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

parse_response(Response, Parser) ->
    {match, Matches} = re:run(Response, Parser, [global, {capture, all_but_first, list}]),
    ToTuple = fun([E,E2|_]) -> {E, E2} end,
    lists:map(ToTuple, Matches).

translate(Data, TranslateMap) when is_list(Data) ->
    string:join(lists:map(fun(X) -> translate(X, TranslateMap) end, Data), "");
translate(_Data={LeftRaw, RightRaw}, TranslateMap) ->
    Left = normalize(LeftRaw),
    Right = normalize(RightRaw),
    case maps:find(Left, TranslateMap) of
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
        {ok, Translator} ->
            Translator;
        error ->
            error_logger:error_msg("No translation for pattern ~p:Val ~p ~n", [Left, Right])
    end.

normalize(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
normalize(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
normalize(String) ->
    String.

send_command_sync(Command, {tcp_serial, Ip, Port}) ->
    serial_tcp_bridge:sync_serial_command({Ip, Port}, Command).

translate_command(Device, Command) ->
    gen_server:call(Device, {translate, Command}).


