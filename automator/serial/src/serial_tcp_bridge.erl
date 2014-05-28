-module(serial_tcp_bridge).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([register_device/3]).
-export([send_command/2]).

-record(serial_tcp_bridge_state, {
          device_map = maps:new() :: map(),
          module_map = maps:new() :: map(),
          ip_socket_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, serial_tcp_bridge}, serial_tcp_bridge, [], []).

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),
    {ok, #serial_tcp_bridge_state{}}.

connect(Ip, Port) ->
    {ok, Socket} = case inet_parse:address(Ip) of
        {ok, ErlangIp} ->
            gen_tcp:connect(ErlangIp, Port, [list, {active, true}]);
        {error, Reason} -> %% Todo .. this could be bad
            throw({error, Reason})
    end,
    Socket.

send_command_to_device(Target, Command, State=#serial_tcp_bridge_state{device_map=DeviceMap}) ->
    case maps:find(Target, DeviceMap) of % Todo Map key should be name Not ip ? Name It?
        {ok, Tcp} ->
            case gen_tcp:send(Tcp, Command) of
                ok ->
                    State;
                {error, _Reason} ->
                    {Ip, Port} = Target,
                    TcpSerial = connect(Ip, Port),
                    State2 = State#serial_tcp_bridge_state{device_map=maps:put({Ip, Port}, TcpSerial, DeviceMap)},
                    State2
            end;
        error ->
            {Ip, Port} = Target,
            TcpSerial = connect(Ip, Port),
            State2 = State#serial_tcp_bridge_state{device_map=maps:put({Ip, Port}, TcpSerial, DeviceMap)},
            send_command_to_device(Target, Command, State2)
    end.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register_device, DeviceModule, Ip, Port}, State=#serial_tcp_bridge_state{
                                                                device_map=DeviceMap,
                                                                module_map=ModuleMap,
                                                                ip_socket_map=IpSocketMap
                                                               }) ->
    State2 = case maps:find({Ip, Port}, DeviceMap) of
        {ok, _Val} ->
            State;
        error ->
            TcpSerial = connect(Ip, Port),

            State#serial_tcp_bridge_state{
              device_map=maps:put({Ip, Port}, TcpSerial, DeviceMap),
              module_map=maps:put({Ip, Port}, DeviceModule, ModuleMap),
              ip_socket_map=maps:put(TcpSerial, {Ip, Port}, IpSocketMap)
            }
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, lists:flatten(Command), State),
    {noreply, State2};
handle_cast({tcp, Socket, Data}, State=#serial_tcp_bridge_state{
                                          module_map=ModuleMap,
                                          ip_socket_map=IpSocketMap
                                         }) ->
    Id = maps:get(Socket, IpSocketMap),
    DeviceModule = maps:get(Id, ModuleMap),
    DeviceModule ! {response, Data},
    {no_reply, State};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

register_device(DeviceModule, Ip, Port) ->
    gen_server:cast(?MODULE, {register_device, DeviceModule, Ip, Port}).

send_command(Target, Command) -> 
    gen_server:cast(?MODULE, {command, Target, Command}).

