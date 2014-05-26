
-module(command_dispatch_handler).

-export([init/3]).

-export([rest_init/2]).
-export([accept_html_get/2]).
-export([accept_html_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%%
-export([allowed_methods/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol , cowboy_rest}.

rest_init(Req, _Opts) ->
    error_logger:error_msg("Got a requuest ~p!~n", [Req]),
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
            error_logger:error_msg("Got a GET!~n"),
            handle_get_status_request(Req2, State);
%%            cowboy_req:qs_vals(Req2);
        {_, Req2} ->
            cowboy_req:reply(404, [], <<"">>, Req2)
    end.

accept_html_post(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
            error_logger:error_msg("Got a POST!~n"),
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
    error_logger:error_msg("What ~p ~p ~p ~n", [Device, Cmd, Req]),
%%    _Result = Device:send({Cmd}),
    {command_parts({Device, Cmd}), cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req3), State}.

handle_post_change_request(Req, State) ->
    {Device, Req2} = cowboy_req:binding(device, Req),
    {Cmd, Req3} = cowboy_req:binding(cmd, Req2),
    {Val, Req4} = cowboy_req:binding(val, Req3),
%%    _Result = Device:send({Cmd, Val}),
    Req5 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, cowboy_req:set_resp_body(command_parts({Device, Cmd, Val}), Req4)),
    {true, Req5, State}.

command_parts(Parts) ->
    io_lib:format("~p~n", [Parts]).

