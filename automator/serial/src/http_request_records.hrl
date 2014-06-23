-include("partial.hrl").

-record(request_body, {
        content_type :: string(),
        data :: list() | {converted, string()}
}).

-record(http_queue_element, {
          token :: term(),
          type :: atom(),
          url :: string(),
          headers :: list(),
          body = #request_body{} :: #request_body{},
          callback :: #partial{},
          start_time :: erlang:timestamp()
}).
%%perform_request(Token, #http_queue_element{token=Token, type=Type, url=Url, headers=Headers, body=Body, callback=Callback, start_time=StartTime}=QueueItem, Pid, State) ->
%%
%%
%%Calls to XBMC should look like
%% Target = {Ip, Port}
%% CallReq = 
%%
%% http_bridge:request_on_queue(xbmc_jsonrpc, get, "http://Ip:Port/jsonrpc", [{"Content-type", "application/json"}], #{ do => TheThing }, FN)
%% http_bridge:request_on_queue(xbmc_jsonrpc, get, "http://Ip:Port/jsonrpc", [{"Content-type", "application/json"}], #{ do => TheThing }, FN)
