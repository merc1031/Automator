-module(receiver_command_handler).

-export([init/3]).

-export([to_html/2]).
%%-export([content_types_provided/2]).


init(_Transport, _Req, []) ->
    {upgrade, protocol , cowboy_rest}.

to_html(Req, State) ->
    {Args, Req3} = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} ->
            cowboy_req:qs_vals(Req2);
        {<<"POST">>, Req2} ->
            cowboy_req:body_qs(Req2)
    end,

    {[<<"You sent me ">>, io_lib:format("~p", [Args])], Req3, State}.


