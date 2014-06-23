-module(http_request_queue_handler).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

-export([request_on_queue/6, complete/3]).

-include("http_request_records.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    error_logger:info_msg("~p:init", [?MODULE]),
    Table = ets:new(http_request_queues, [named_table, public, set]),
    {ok, Table}.

handle_call(_Message, _From, _State) ->
    error_logger:error_msg("~p does not implement handle_call", [?MODULE]),
    {erlang:throw({error, {?MODULE, unhandled_function}})}.

handle_cast({request_on_queue, Token, Type, Url, Headers, Body, Callback, StartTime}, State) ->
    QueueItem = #http_queue_element{token=Token, type=Type, url=Url, headers=Headers, body=Body, callback=Callback, start_time=StartTime},
    case ets:lookup(State, Token) of
        [{Token, RequestQueue}] when is_list(RequestQueue) ->
            error_logger:info_msg("Queue a request on Token ~p", [Token]),
            NewQueue = RequestQueue ++ [QueueItem],
            ets:insert(State, {Token, NewQueue});
        [] ->
            error_logger:info_msg("Nothing waiting perform the request", []),
            perform_request(QueueItem, State);
        Any ->
            error_logger:info_msg("There is badly formed data ~p in the queue for ~p", [Any, Token])
    end,
    {noreply, State};
handle_cast({complete, FromPid, Token, _Result}, State) ->
    error_logger:info_msg("We completed a request for token ~p", [Token]),
    [{Token, [#http_queue_element{} | Tail]}] = ets:lookup(State, Token),
    case Tail of
        [] ->
            error_logger:info_msg("None remaining delete ets ~p", [Token]),
            ets:delete(State, Token),
            automator_http_request:stop(FromPid);
        [#http_queue_element{}=QueueItem | _] ->
            error_logger:info_msg("Waiting request on complete gogo", []),
            perform_request(QueueItem, FromPid, State);
        Any ->
            error_logger:info_msg("There is badly formed data ~p in the queue for ~p", [Any, Token]),
            throw({bad_queued_request_element, Any})
    end,
    {noreply, State}.

handle_info(_Message, _State) ->
    error_logger:error_msg("~p does not implement handle_info", [?MODULE]),
    {erlang:throw({error, {?MODULE, unhandled_function}})}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, State) ->
    ets:delete(State),
    ok.


perform_request(#http_queue_element{}=QueueItem, State) ->
    {ok, Pid} = automator_http_request_sup:start_child(),
    perform_request(QueueItem, Pid, State).

perform_request(#http_queue_element{token=Token}=QueueItem, Pid, State) ->
    ets:insert(State, {Token, [QueueItem]}),
    try
        automator_http_request:perform_request(Pid, QueueItem)
    catch
        Type:Error ->
            automator_http_request:request_failure(Pid, QueueItem, {Type, Error})
    end.

request_on_queue(Token, Type, Url, Headers, Body, Callback) ->
    error_logger:info_msg("Queuing a request to Token ~p", [Token]),
    try
        gen_server:cast(?MODULE, {request_on_queue, Token, Type, Url, Headers, Body, Callback, erlang:now()})
    catch
        error:Any ->
            error_logger:error_msg("Failed to create the http request: ~p", [Any]);
        Any ->
            Any
    end,
    Token.

complete(FromPid, Token, Result) ->
    gen_server:cast(?MODULE, {complete, FromPid, Token, Result}).
