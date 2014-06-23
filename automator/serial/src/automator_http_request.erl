-module(automator_http_request).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-export([stop/1]).
-export([perform_request/2, request_failure/3]).
-export([async_complete/5]).


-include("http_request_records.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, none}.

stop(Pid) ->
    gen_server:cast(Pid, stop).



perform_request(Pid, QueueItem=#http_queue_element{}) ->
    gen_server:cast(Pid, {perform_request, QueueItem}).

request_failure(Pid, QueueItem=#http_queue_element{}, Error) ->
    gen_server:cast(Pid, {request_failure, QueueItem, Error}).



handle_call(Message, From, _State) ->
    error_logger:error_msg("~p:handle_call is unhandled: Message ~p from ~p", [?MODULE, Message, From]),
    throw({errror, unhandled_function}).

handle_cast({perform_request, #http_queue_element{}=Request}, State) ->
    make_request(Request),
    {noreply, State};
handle_cast({request_failure, #http_queue_element{}=Request, Error}, State) ->
    complete(Request, Error),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Message, _State) ->
    error_logger:error_msg("~p:handle_info is unhandled: Message ~p", [?MODULE, Message]),
    throw({errror, unhandled_function}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


make_request(QueueItem=#http_queue_element{type=Type, url=Url, body=Body}) ->
   UrlC = maybe_handle_url_args(Type, Url, Body),
   BodyC = maybe_handle_body(Type, Body),
   http_request(QueueItem#http_queue_element{url=UrlC, body=BodyC}).

http_request(#http_queue_element{type=post,
                                 token=Token,
                                 url=Url,
                                 headers=Headers,
                                 body=#request_body{
                                         content_type=ContentType,
                                         data={converted, Data}},
                                 callback=Callback}) ->
    error_logger:info_msg("Making a post request ~p ~p ~p ~p", [lists:flatten(Url), Headers, ContentType, Data]),
    {ok, RId} = httpc:request(post,
                  {lists:flatten(Url), Headers, ContentType, Data},
                  [{version, "HTTP/1.0"}],
                  [
                   {sync, false},
                   {receiver,
                    {automator_http_request, async_complete,
                     [self(), Callback, Token, {Url, Data}]
                    }
                   }
                  ]
                 ),
    error_logger:info_msg("RequestId is ~p", [RId]);
http_request(#http_queue_element{type=get,
                                 token=Token,
                                 url=Url,
                                 headers=Headers,
                                 callback=Callback}) ->
    httpc:request(get,
                  {Url, Headers},
                  [{version, "HTTP/1.0"}],
                  [
                   {sync, false},
                   {receiver,
                    {automator_http_request, async_complete,
                     [self(), Callback, Token, {Url, ""}]
                    }
                   }
                  ]
                 ).

complete(#http_queue_element{callback=Callback, token=Token}, Result) ->
    try
        partial:call(Callback, [Token, Result])
    catch
        Error:Type ->
            error_logger:error_msg("Http request from queue ~p failed with ~p:~p with context ~p", [Token, Error, Type, Callback])
    end,
    http_request_queue_handler:complete(self(), Token, Result).

async_complete({_HttpRequestId, Result}, Pid, Callback, Token, Req) ->
    error_logger:info_msg("We should be getting a response now ~p", [Result]),
    try
        partial:call(Callback, [Token, Result, Req])
    catch
        Error:Type ->
            error_logger:error_msg("Http request from queue ~p failed in async with ~p:~p with context ~p", [Token, Error, Type, Callback])
    end,
    http_request_queue_handler:complete(Pid, Token, Result).




maybe_handle_url_args(post, Url, _Args) ->
    Url;
maybe_handle_url_args(get, Url, Args) ->
    Url ++ url_encode_args(Args).

url_encode_args(#request_body{data=[]}) ->
    "";
url_encode_args(Args=#request_body{content_type="application/json"}) ->
    "?" ++ mochiweb_util:urlencode(lists:map(fun({X, Y}) -> {X, jiffy:encode(Y)} end, Args#request_body.data)).

maybe_handle_body(get, Args) when is_record(Args, request_body) ->
    Args#request_body{data={converted, ""}};
maybe_handle_body(post, Args) when is_record(Args, request_body) ->
    Args#request_body{data=convert_by_type(Args)};
maybe_handle_body(post, Args) ->
    maybe_handle_body(post, #request_body{data=Args, content_type="application/json"}); %todo make this passed in
maybe_handle_body(get, Args) ->
    maybe_handle_body(get, #request_body{data=Args}). %todo make this passed in

convert_by_type(#request_body{content_type="application/json", data=Data}) ->
    {converted, binary_to_list(jiffy:encode(Data))}.




