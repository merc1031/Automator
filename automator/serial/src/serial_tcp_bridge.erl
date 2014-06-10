-module(serial_tcp_bridge).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([register_device/4]).
-export([send_command/2]).

-record(serial_tcp_bridge_state, {
          device_map = maps:new() :: map(),
          module_map = maps:new() :: map(),
          ip_socket_map = maps:new() :: map(),
          name_to_data_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, serial_tcp_bridge}, serial_tcp_bridge, [], []).

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),
    {ok, #serial_tcp_bridge_state{}}.

connect(Ip, Port) ->
    {{ok, Socket}, EIp} = case inet_parse:address(Ip) of
        {ok, ErlangIp} ->
            {gen_tcp:connect(ErlangIp, Port, [binary, {active, true}]), ErlangIp};
        {error, Reason} -> %% Todo .. this could be bad
            throw({error, Reason})
    end,
    {Socket, EIp}.

send_command_to_device(Target, Command, State=#serial_tcp_bridge_state{device_map=DeviceMap}) ->
    case maps:find(Target, DeviceMap) of % Todo Map key should be name Not ip ? Name It?
        {ok, Tcp} ->
            case gen_tcp:send(Tcp, Command) of
                ok ->
                    State;
                {error, _Reason} ->
                    {Ip, Port} = Target,
                    {TcpSerial, _EIp} = connect(Ip, Port),
                    State2 = State#serial_tcp_bridge_state{device_map=maps:put({Ip, Port}, TcpSerial, DeviceMap)},
                    State2
            end;
        error ->
            throw({?MODULE, io_lib:format("Could not find Target ~p for Command ~p", [Target, Command])})
    end.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register_device, _From, DeviceModule, Ip, Port}, State=#serial_tcp_bridge_state{
                                                                device_map=DeviceMap,
                                                                module_map=ModuleMap,
                                                                ip_socket_map=IpSocketMap,
                                                                name_to_data_map=NameToDataMap
                                                               }) ->
    State2 = case maps:find({Ip, Port}, DeviceMap) of
        {ok, _Val} ->
            State;
        error ->
            {TcpSerial, EIp} = connect(Ip, Port),
           
            Uid = list_to_atom(tuple_to_list(EIp) ++ integer_to_list(Port)),
            State#serial_tcp_bridge_state{
              device_map=maps:put({Ip, Port}, TcpSerial, DeviceMap),
              module_map=maps:put({Ip, Port}, DeviceModule, ModuleMap),
              ip_socket_map=maps:put(TcpSerial, {Ip, Port}, IpSocketMap),
              name_to_data_map=maps:put(Uid, #{socket=>TcpSerial, ip_s=>Ip, port=>Port, ip=>EIp}, NameToDataMap)
            }
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, lists:flatten(Command), State),
    {noreply, State2};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info({tcp, Socket, Data}, State=#serial_tcp_bridge_state{
                                          module_map=ModuleMap,
                                          ip_socket_map=IpSocketMap
                                         }) ->
    error_logger:error_msg("TcpSerial got a response ~p", [Data]),
    Id = maps:get(Socket, IpSocketMap),
    DeviceModule = maps:get(Id, ModuleMap),
    error_logger:error_msg("Got module ~p for ~p mapped to ~p", [DeviceModule, Socket, Id]),
    DeviceModule ! {response, Data},
    {noreply, State};
handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

register_device(From, DeviceModule, Ip, Port) ->
    gen_server:cast(?MODULE, {register_device, From, DeviceModule, Ip, Port}).

send_command(Target, Command) -> 
    error_logger:error_msg("Getting a command casted ~p ~p", [Target, Command]),
    gen_server:cast(?MODULE, {command, Target, Command}).

