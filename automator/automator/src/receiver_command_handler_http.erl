
-module(receiver_command_handler_http).

-export([init/3]).

-export([to_html/2, to_text/2, to_json/2]).
-export([from_html/2, from_text/2, from_json/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%%
-export([allowed_methods/2]).


init(_Transport, _Req, []) ->
    {upgrade, protocol , cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, to_html},
      {<<"application/json">>,to_json},
      {<<"text/plain">>, to_text}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"text/html">>, from_html},
      {<<"application/json">>,from_json},
      {<<"text/plain">>, from_text}
     ], Req, State}.

to_html(Req, State) ->
    {Args, Req3} = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            cowboy_req:qs_vals(Req2);
        {<<"POST">>, Req2} ->
            {ok, T, Z} = cowboy_req:body_qs(Req2),
            {T, Z}
    end,

    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"You wanted html, You sent me ">>, io_lib:format("~p", [Args])])]),
    {iolist_to_binary([<<"<html> <head></head> <body>You wanted HTML, You sent me ">>, io_lib:format("~p", [Args]), <<"</body></html>">>]), Req3, State}.


to_json(Req, State) ->
    {Args, Req3} = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            cowboy_req:qs_vals(Req2);
        {<<"POST">>, Req2} ->
            {ok, T, Z} = cowboy_req:body_qs(Req2),
            {T, Z}
    end,

    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"You wanted json, You sent me ">>, io_lib:format("~p", [Args])])]),
    {iolist_to_binary([<<"{\"wanted\": \"You wanted json\", \"sent\": \"You sent me ">>, io_lib:format("~p", [Args]), <<" \"">>]), Req3, State}.

to_text(Req, State) ->
    {Args, Req3} = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            cowboy_req:qs_vals(Req2);
        {<<"POST">>, Req2} ->
            {ok, T, Z} = cowboy_req:body_qs(Req2),
            {T, Z}
    end,

    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"You wanted Text, You sent me ">>, io_lib:format("~p", [Args])])]),
    {iolist_to_binary([<<"You wanted Text, You sent me ">>, io_lib:format("~p", [Args])]), Req3, State}.

from_html(Req, State) ->
    {ok, Args, Req2} = cowboy_req:body_qs(Req),

    {ok, Req3} = cowboy_req:set_resp_body(iolist_to_binary([<<"<html> <head></head> <body>FROM You wanted HTML), You sent me ">>, io_lib:format("~p", [Args]), <<"</body></html>">>]), Req2),
    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"FROM You wanted html, You sent me ">>, io_lib:format("~p", [Args])])]),
    {true, Req3, State}.


from_json(Req, State) ->
    {ok, Args, Req2} = cowboy_req:body_qs(Req),

    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"FROM You wanted json, You sent me ">>, io_lib:format("~p", [Args])])]),
    {ok, Req3} = cowboy_req:set_resp_body(iolist_to_binary([<<"{\"wanted\": \"FROM You wanted json\", \"sent\": \"You sent me ">>, io_lib:format("~p", [Args]), <<" \"}">>]), Req2),
    {true, Req3, State}.

from_text(Req, State) ->
    {ok, Args, Req2} = cowboy_req:body_qs(Req),

    error_logger:error_msg("Got a reqest! ~p~n", [iolist_to_binary([<<"FROM You wanted text, You sent me ">>, io_lib:format("~p", [Args])])]),
    {ok, Req3} = cowboy_req:set_resp_body(iolist_to_binary([<<"FROM You wanted Text, You sent me ">>, io_lib:format("~p", [Args])]), Req2),
    {true, Req3, State}.

