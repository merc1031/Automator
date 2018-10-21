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
          module_map = maps:new() :: map(),
          name_to_data_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, serial_tcp_bridge}, serial_tcp_bridge, [], []).

init([]) ->
    error_logger:info_msg("~p ~p ~p:init()", [?MODULE, self(), ?MODULE]),
    {ok, #serial_tcp_bridge_state{}}.

connect(ErlangIp, Port) ->
    case gen_tcp:connect(ErlangIp, Port, [binary, {active, true}]) of
        {ok, InnerSocket} ->
            InnerSocket;
        {error, Reason} -> %% Todo .. this could be bad
            throw({error, Reason})
    end.

send_command_to_device(Target, Command, State=#serial_tcp_bridge_state{
                                                 name_to_data_map=NameToDataMap
                                                }) ->
    case maps:find(Target, NameToDataMap) of % Todo Map key should be name Not ip ? Name It?
        {ok, #{socket:=Tcp}} ->
            case gen_tcp:send(Tcp, Command) of
                ok ->
                    State;
                {error, Reason} ->
                    throw({?MODULE, io_lib:format("Could not send to Target ~p for Command ~p for reason ~p", [Target, Command, Reason])}) %% maybe instead of throw re-request register? keep track of who wanted this?
            end;
        error ->
            throw({?MODULE, io_lib:format("Could not find Target ~p for Command ~p", [Target, Command])})
    end.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast({register_device, From, DeviceModule, Ip, Port}, State=#serial_tcp_bridge_state{
                                                                module_map=ModuleMap,
                                                                name_to_data_map=NameToDataMap
                                                               }) ->
    {ok, ErlangIp} = inet_parse:address(Ip),
    Uid = list_to_atom(tuple_to_list(ErlangIp) ++ integer_to_list(Port) ++ atom_to_list(DeviceModule)),
    State2 = case maps:find(Uid, NameToDataMap) of
        {ok, _Val} ->
            From ! {registered, Uid},
            State;
        error ->
            Socket = connect(ErlangIp, Port),
            From ! {registered, Uid},
            State#serial_tcp_bridge_state{
              module_map=maps:put(Socket, DeviceModule, ModuleMap),
              name_to_data_map=maps:put(Uid, #{socket=>Socket, raw_ip=>Ip, port=>Port, ip=>ErlangIp}, NameToDataMap)
            }
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, lists:flatten(Command), State),
    {noreply, State2};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info({tcp, Socket, Data}, State=#serial_tcp_bridge_state{
                                          module_map=ModuleMap
                                         }) ->
    DeviceModule = maps:get(Socket, ModuleMap),
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
    gen_server:cast(?MODULE, {command, Target, Command}).

