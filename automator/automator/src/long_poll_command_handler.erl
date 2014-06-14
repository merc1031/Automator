-module(long_poll_command_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(http_state, {
         grace_timeout :: non_neg_integer()
}).

init({_Transport, http}, Req, _Opts) ->
    %process_flag(trap_exit, true),
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    {Val, Req4} = cowboy_req:binding(val, Req3, ""),
    error_logger:error_msg("Request incoming from ~p ~p", [self(), {Device, Cmd, Val}]),
    GraceTimeout = device:query_timeout(Device, Cmd),
    ShouldWait = device:query_should_wait(Device, Cmd),
    error_logger:error_msg("We have queried the following for device ~p : Wait ~p Timout ~p", [Device, ShouldWait, GraceTimeout]),
    device:translate_command(self(), Device, {Cmd, Val}),
    case ShouldWait of
        yes ->
            {loop, Req4, #http_state{grace_timeout=GraceTimeout}, 20000, hibernate};
        no ->
            {shutdown, Req4, #http_state{}}
    end.

info({response, Translated}, Req, State) ->
    ReqF = receive
        {response, MoreData} ->
            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], format_return(lists:concat([Translated, MoreData])), Req),
            Req2
    after State#http_state.grace_timeout ->
            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], format_return(Translated), Req),
            Req2
    end,
    {ok, ReqF, State};
%info({'EXIT', _Pid, _Reason}, Req, State) ->
%    {ok, Req2} = cowboy_req:reply(200, [], IoList, Req),
%    {ok, Req2, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.

format_return(Result) ->
    io_lib:format("~s", [Result]).
