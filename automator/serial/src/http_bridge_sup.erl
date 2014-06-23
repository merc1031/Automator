-module(http_bridge_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_) ->
    HttpBridge = {http_bridge,
                  {http_bridge, start_link, []},
                 permanent, 5000, worker, [http_bridge]},
    HttpRequestQueueHandler = {http_request_queue_handler,
             {http_request_queue_handler, start_link, []},
             permanent, 5000, worker, [http_request_queue_handler]},

    HttpRequestSup = {automator_http_request_sup,
             {automator_http_request_sup, start_link, []},
             permanent, 5000, worker, [automator_http_request_sup]},

    {ok, {{one_for_one, 5, 60}, [HttpBridge, HttpRequestQueueHandler, HttpRequestSup]}}.
