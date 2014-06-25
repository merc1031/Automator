-module(udp_bridge).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([register_device/5]).
-export([send_command/2]).

-record(udp_bridge_state, {
          name_to_data_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, udp_bridge}, udp_bridge, [], []).

init([]) ->
    error_logger:info_msg("~p ~p: ~p:init()", [?MODULE, self(), ?MODULE]),
    {ok, #udp_bridge_state{}}.

connect() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, Port} = inet:port(Socket),
    #{udp_port => Port, socket => Socket}.

send_command_to_device(Target, Command, State=#udp_bridge_state{
                                                 name_to_data_map=NameToDataMap
                                                }) ->
    case maps:find(Target, NameToDataMap) of
        {ok, #{socket := Socket, ip := Ip, port := Port}=_Opts} ->
            case gen_udp:send(Socket, Ip, Port, Command) of
                ok ->
                    State;
                {error, Reason} ->
                    throw({?MODULE, io_lib:format("Could not send to Target ~p for Command ~p for reason ~p", [Target, Command, Reason])})
            end;
        error ->
            error_logger:error_msg("~p ~p: Target data not found for ~p. This should never happen", [?MODULE, self(), Target])
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register_device, From, DeviceModule, Ip, Port, Opts=#{init:=InitPackets, keepalive:=KeepAliveConfig}}, State=#udp_bridge_state{
                                                                name_to_data_map=NameToDataMap
                                                               }) ->
    {ok, ErlangIp} = inet:parse_address(Ip),
    Uid = list_to_atom(tuple_to_list(ErlangIp) ++ integer_to_list(Port)),
    State2 = case maps:find(Uid, NameToDataMap) of
        {ok, _Val} ->
            State;
        error ->
            PortAndSocket = connect(),

            NewOpts = maps:merge(PortAndSocket, #{raw_ip=> Ip, ip => ErlangIp, port => Port, uid => Uid, device_module => DeviceModule}),
            IntOpts = maps:merge(Opts, NewOpts),
            handle_init(InitPackets, IntOpts),
            Timer = set_keepalive(KeepAliveConfig, IntOpts),
            From ! {registered, Uid},
            State#udp_bridge_state{
              name_to_data_map=maps:put(Uid, maps:merge(IntOpts, #{timer => Timer}), NameToDataMap)
            }
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, Command, State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({keepalive, Uid, KeepAlivePackets}, State=#udp_bridge_state{name_to_data_map=NameToDataMap}) ->
    case maps:find(Uid, NameToDataMap) of
        {ok, #{ socket := Socket, ip := EIp, port := Port }} ->
            lists:foreach(fun(Packet) -> ok = gen_udp:send(Socket, EIp, Port, Packet) end, KeepAlivePackets);
        error ->
            error_logger:error_msg("~p ~p: Triggerd keepalive for ~p but couldnt find it", [?MODULE, self(), Uid])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_init(InitPackets, #{ socket := Socket, ip := Ip, port := Port}) ->
    lists:foreach(fun(Packet) -> ok = gen_udp:send(Socket, Ip, Port, Packet) end, InitPackets).

set_keepalive(KeepAliveConfig, #{uid := Uid}) ->
    {KeepAlivePeriod, KeepAlivePackets} = KeepAliveConfig,
    {ok, Timer} = timer:send_interval(KeepAlivePeriod, {keepalive, Uid, KeepAlivePackets}),
    Timer.

register_device(From, DeviceModule, Ip, Port, Opts=#{}) ->
    gen_server:cast(?MODULE, {register_device, From, DeviceModule, Ip, Port, Opts}).

send_command(Target, Command) ->
    gen_server:cast(?MODULE, {command, Target, Command}).

