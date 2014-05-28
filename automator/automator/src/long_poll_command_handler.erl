-module(long_poll_command_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-record(http_state, {
         grace_period = false :: true | false,
         buffer = "" :: list()
}).

init({_Transport, http}, Req, _Opts) ->
    %process_flag(trap_exit, true),
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    {Val, Req4} = cowboy_req:binding(val, Req3, ""),
    error_logger:error_msg("Request incoming from ~p ~p", [self(), {Device, Cmd, Val}]),
    device:translate_command(self(), Device, {Cmd, Val}),
    {loop, Req4, #http_state{}, 20000, hibernate}.

info({response, Translated}, Req, State) ->
    ReqF = receive
        {response, MoreData} ->
            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], format_return(lists:concat(Translated, MoreData)), Req),
            Req2
    after 500 ->
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
