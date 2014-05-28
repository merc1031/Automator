-module(long_poll_command_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).


init({_Transport, http}, Req, _Opts) ->
    %process_flag(trap_exit, true),
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    {Val, Req4} = cowboy_req:binding(val, Req3, ""),
    device:translate_command(self(), Device, {Cmd, Val}),
    {loop, Req4, #{}, 20000, hibernate}.

info({reply, {response, Translated}}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], format_return(Translated), Req),
    {ok, Req2, State};
%info({'EXIT', _Pid, _Reason}, Req, State) ->
%    {ok, Req2} = cowboy_req:reply(200, [], IoList, Req),
%    {ok, Req2, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.

format_return(Result) ->
    io_lib:format("~s", [Result]).
