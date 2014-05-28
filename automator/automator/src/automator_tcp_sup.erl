-module(automator_tcp_sup).

-export([start_link/0]).
-behaviour(supervisor).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),
    Port = application:get_env(automator, tcp_frontend_port, 9375),
    TcpListener = ranch:child_spec(automator_tcp_listener, 5, ranch_tcp, [{port, Port}, {max_connections, 100}], text_protocol, []),

    {ok, {{one_for_all, 10, 10}, [TcpListener]}}.

