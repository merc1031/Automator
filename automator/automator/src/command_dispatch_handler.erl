
-module(command_dispatch_handler).

-export([init/3]).

-export([rest_init/2]).
-export([accept_html_get/2]).
-export([accept_html_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([allowed_methods/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol , cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, 10}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, accept_html_get}
     ], Req, State}.


content_types_accepted(Req, State) ->
    {[
      {{<<"text">>, <<"plain">>, '*'}, accept_html_post}
     ], Req, State}.

accept_html_get(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            handle_get_status_request(Req2, State);
%%            cowboy_req:qs_vals(Req2);
        {_, Req2} ->
            cowboy_req:reply(404, [], <<"">>, Req2)
    end.

accept_html_post(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
            handle_post_change_request(Req2, State);
%%            {ok, T, Z} = cowboy_req:body_qs(Req2),
%%            {T, Z}
        {_, Req2} ->
            cowboy_req:reply(404, [], <<"">>, Req2)
    end.


handle_get_status_request(Req, State) ->
%    {Args, Req2} = cowboy_req:qs_vals(Req),
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    Result = device:translate_command(Device, {Cmd, ""}),
    error_logger:info_msg(Result),
    {format_return(Result), cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req3), State}.

handle_post_change_request(Req, State) ->
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    {Val, Req4} = cowboy_req:binding(val, Req3, ""),
    Result = device:translate_command(Device, {Cmd, Val}),
    error_logger:info_msg(Result),
    Req5 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, cowboy_req:set_resp_body(format_return(Result), Req4)),
    {true, Req5, State}.

format_return(Result) ->
    io_lib:format("~s", [Result]).

