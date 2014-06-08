-module(automator_web_sup).

-export([start_link/0]).
-behaviour(supervisor).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),
    Dispatch = cowboy_router:compile([
                        {'_', [
                            {"/serial/:device/[:cmd/[:val]]", [{device, function, fun _Atomize(Val) -> {true, binary_to_atom(Val, latin1)} end}], long_poll_command_handler, []},
                            {"/udp/:device/[:cmd/[:val]]", [{device, function, fun _Atomize(Val) -> {true, binary_to_atom(Val, latin1)} end}], long_poll_command_handler, []}
                        ]}
                ]),
    Port = application:get_env(automator, http_frontend_port, 9374),
    WebListener = ranch:child_spec(automator_web_listener, 5, ranch_tcp, [{port, Port}, {max_connections, 100}], cowboy_protocol, [{env, [{dispatch, Dispatch}]}]),

    {ok, {{one_for_all, 10, 10}, [WebListener]}}.

