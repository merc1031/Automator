-module(automator_http_request_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    HttpRequest = { automator_http_request, {automator_http_request, start_link, []},
                        temporary, 5000, worker, [automator_http_request]},
    {ok, {{simple_one_for_one, 5, 60}, [HttpRequest]}}.
