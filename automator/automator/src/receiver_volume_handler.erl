
-module(receiver_volume_handler).

-export([init/3]).

-export([rest_init/2]).
-export([accept_html_get/2]).
-export([accept_html_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%%
-export([allowed_methods/2]).

-export([set_vol/2, inc_vol/2, dec_vol/2]).


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
            handle_volume_status_request(Req2, State);
%%            cowboy_req:qs_vals(Req2);
        {_, Req2} ->
            cowboy_req:reply(404, [], <<"">>, Req2)
    end.

accept_html_post(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} ->
            error_logger:error_msg("Got a POST!~n"),
            handle_volume_change_request(Req2, State);
%%            {ok, T, Z} = cowboy_req:body_qs(Req2),
%%            {T, Z}
        {_, Req2} ->
            cowboy_req:reply(404, [], <<"">>, Req2)
    end.


handle_volume_status_request(Req, State) ->
%    {Args, Req2} = cowboy_req:qs_vals(Req),
    error_logger:error_msg("Got a reqest for volume!~n"),
    {volume_status(State), cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req), State}.

handle_volume_change_request(Req, State) ->
    {Val, Req2} = cowboy_req:binding(cmd, Req),
    error_logger:error_msg("Got a reqest for ~p~n", [Val]),
    ?MODULE:Val(Req2, State).

set_vol(Req, _State) ->
    {Val, Req2} = cowboy_req:binding(vol, Req),
%    {ok, Args, Req2} = cowboy_req:body_qs(Req),
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, cowboy_req:set_resp_body(volume_status(Val), Req2)),
    {true, Req3, Val}.

inc_vol(Req, State) ->
    {Val, Req2} = cowboy_req:binding(vol, Req),
%    {ok, Args, Req2} = cowboy_req:body_qs(Req),
    StateNew = State + Val,
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, cowboy_req:set_resp_body(volume_status(StateNew), Req2)),
    {true, Req3, StateNew}.

dec_vol(Req, State) ->
    {Val, Req2} = cowboy_req:binding(vol, Req),
%    {ok, Args, Req2} = cowboy_req:body_qs(Req),
    StateNew = State - Val,
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, cowboy_req:set_resp_body(volume_status(StateNew), Req2)),
    {true, Req3, StateNew}.

volume_status(State) ->
    io_lib:format("VOL~p~n", [State]).

