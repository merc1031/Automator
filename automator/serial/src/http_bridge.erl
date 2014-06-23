-module(http_bridge).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

-export([register_device/5, send_command/2]).
-export([req_complete/5]).



-record(http_bridge_state, {
          name_to_data_map = maps:new() :: map()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    error_logger:info_msg("~p:init", [?MODULE]),
    {ok, #http_bridge_state{}}.

handle_call(_Message, _From, _State) ->
    error_logger:error_msg("~p does not implement handle_call", [?MODULE]),
    {erlang:throw({error, {?MODULE, unhandled_function}})}.

handle_cast({register_device, From, DeviceModule, Ip, Port, Opts=#{
                                                              req_type := Type,
                                                              url_format := UrlFormat
                                                             }},
            State=#http_bridge_state{
                    name_to_data_map=NameToDataMap
                    }) ->
    error_logger:info_msg("http_bridge init a device", []),
    BaseUrl = io_lib:format(UrlFormat, [Ip, Port]),
    Uid = list_to_atom(lists:flatten(BaseUrl ++ atom_to_list(Type))),
    error_logger:info_msg("http_bridge init a device with id ~p", [Uid]),
    State2 = case maps:find(Uid, NameToDataMap) of
                 {ok, _Val} ->
                     State;
                 error ->
                     NewOpts = #{ base_url => BaseUrl, ip => Ip, port => Port, device_module => DeviceModule},
                     IntOpts = maps:merge(Opts, NewOpts),
                     From ! {registered, Uid},
                     State#http_bridge_state{
                      name_to_data_map=maps:put(Uid, IntOpts, NameToDataMap)}
    end,
    {noreply, State2};
handle_cast({command, Target, Command}, State) ->
    State2 = send_command_to_device(Target, Command, State),
    {noreply, State2}.

handle_info(_Message, _State) ->
    error_logger:error_msg("~p does not implement handle_info", [?MODULE]),
    {erlang:throw({error, {?MODULE, unhandled_function}})}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _State) ->
    ok.

send_command_to_device(Target, Command,
            State=#http_bridge_state{
                    name_to_data_map=NameToDataMap
                    }) ->
    case maps:find(Target, NameToDataMap) of
        {ok, #{base_url := BaseUrl, device_module := DeviceModule, req_type := Type, headers := Headers}=_Opts} ->
            http_request_queue_handler:request_on_queue(Target, Type, BaseUrl, Headers, Command, partial:make(?MODULE, req_complete, [Target, DeviceModule]));
        error ->
            error_logger:error_msg("This is bad.", [])
    end,
    State.


req_complete(Target, DeviceModule, Token, {{_,200,_}, _H, ResultBody}=Result, Req) ->
    error_logger:info_msg("We got a response in http bridge! ~p ~p ~p ~p", [Target, DeviceModule, Token, Result]),
    DeviceModule ! {response, {ResultBody, Req}}.

register_device(From, DeviceModule, Ip, Port, Opts=#{}) ->
    error_logger:info_msg("We are attempting to register a http_bridge target ~p ~p", [DeviceModule, Opts]),
    gen_server:cast(?MODULE, {register_device, From, DeviceModule, Ip, Port, Opts}).

send_command(Target, Command) ->
    error_logger:info_msg("We got a command in http_bridge ~p", [Command]),
    gen_server:cast(?MODULE, {command, Target, Command}).
