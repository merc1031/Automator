-module(automator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    error_logger:info_msg("~p:init()~n", [?MODULE]),

    WebSupervisor = {automator_web_sup,
                     {automator_web_sup, start_link, []},
                    permanent, 5000, supervisor, [automator_web_sup]},
    TcpSupervisor = {automator_tcp_sup,
                     {automator_tcp_sup, start_link, []},
                    permanent, 5000, supervisor, [automator_tcp_sup]},
    {ok, { {one_for_one, 10, 10}, [WebSupervisor, TcpSupervisor]} }.

