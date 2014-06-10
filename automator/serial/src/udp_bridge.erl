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
          device_map = maps:new() :: map(),
          module_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, udp_bridge}, udp_bridge, [], []).

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),
    {ok, #udp_bridge_state{}}.

connect() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Port = inet:port(Socket),
    #{udp_port => Port, socket => Socket}.

send_command_to_device(Target, Command, State=#udp_bridge_state{device_map=DeviceMap}) ->
    case maps:find(Target, DeviceMap) of % Todo Map key should be name Not ip ? Name It?
        {ok, #{socket := Socket, ip := Ip, port := Port}=Opts} ->
            case gen_udp:send(Socket, Ip, Port, Command) of
                ok ->
                    State;
                {error, _Reason} ->
                    #{ timer := OldTimer, init := InitPackets, keepalive := KeepAliveConfig } = Opts,
                    timer:cancel(OldTimer),

                    PortAndSocket = connect(),
                    IntOpts = maps:merge(Opts, PortAndSocket),
                    handle_init(InitPackets, IntOpts),
                    Timer = set_keepalive(KeepAliveConfig, IntOpts),
                    State2 = State#udp_bridge_state{device_map=maps:put(Target, maps:merge(IntOpts, #{ timer => Timer }), DeviceMap)},
                    State2
            end;
        error ->
            error_logger:error_msg("This is bad.", [])
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register_device, _From, DeviceModule, Ip, Port, Opts=#{init:=InitPackets, keepalive:=KeepAliveConfig}}, State=#udp_bridge_state{
                                                                device_map=DeviceMap,
                                                                module_map=ModuleMap
                                                               }) ->
    State2 = case maps:find({Ip, Port}, DeviceMap) of
        {ok, _Val} ->
            State;
        error ->
            PortAndSocket = connect(),
            {ok, ErlangIp} = inet:parse_address(Ip),

            NewOpts = maps:merge(PortAndSocket, #{raw_ip=> Ip, ip => ErlangIp, port => Port}),
            IntOpts = maps:merge(Opts, NewOpts),
            handle_init(InitPackets, IntOpts),
            Timer = set_keepalive(KeepAliveConfig, IntOpts),
            State#udp_bridge_state{
              device_map=maps:put({Ip, Port}, maps:merge(IntOpts, #{timer => Timer}), DeviceMap),
              module_map=maps:put({Ip, Port}, DeviceModule, ModuleMap)
            }
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, lists:flatten(Command), State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({keepalive, Ip, Port, KeepAlivePackets}, State=#udp_bridge_state{device_map=DeviceMap}) ->
    case maps:find({Ip, Port}, DeviceMap) of
        {ok, #{ socket := Socket, ip := EIp }} ->
            lists:foreach(fun(Packet) -> ok = gen_udp:send(Socket, EIp, Port, Packet) end, KeepAlivePackets);
        error ->
            error_logger:error_msg("BAD", [])
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

set_keepalive(KeepAliveConfig, #{ raw_ip := Ip, port := Port }) ->
    {KeepAlivePeriod, KeepAlivePackets} = KeepAliveConfig,
    {ok, Timer} = timer:send_interval(KeepAlivePeriod, {keepalive, Ip, Port, KeepAlivePackets}),
    Timer.

register_device(From, DeviceModule, Ip, Port, Opts=#{}) ->
    gen_server:cast(?MODULE, {register_device, From, DeviceModule, Ip, Port, Opts}).

send_command(Target, Command) -> 
    error_logger:error_msg("Getting a command casted ~p ~p", [Target, Command]),
    gen_server:cast(?MODULE, {command, Target, Command}).

