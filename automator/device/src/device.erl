-module(device).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([translate/2]).

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

handle_call({translate, Command}, _From, State=#device_state{target=Target}) ->
        error_logger:error_msg("In ~p ~p", [State#device_state.name, Command]),
        Result = send_command_sync(Command, Target),
        {reply, Result, State};
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

send_command_sync(Command, {tcp_serial, Ip, Port}) ->
    serial_tcp_bridge:sync_serial_command({Ip, Port}, Command).

translate(Device, Command) ->
    gen_server:call(Device, {translate, Command}).


