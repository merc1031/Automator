-module(text_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4]).

-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 60000).

-record(state, {socket, transport}).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    error_logger:info_msg("~p:init ~p", [?MODULE, self()]),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
                          #state{socket=Socket, transport=Transport}).

handle_info({tcp, _Socket, Data}, State=#state{
                                          socket=ReplySocket,
                                          transport=Transport}) ->
    error_logger:info_msg("~p ~p got a tcp request ~p", [?MODULE, self(), Data]),
    Datas = binary:split(Data, <<"\r\n">>, [global]),
    ShouldWaits = lists:map(fun send_to_target/1, Datas),
    case lists:any(fun (X) -> X =:= yes end, ShouldWaits) of
        false ->
            Transport:setopts(ReplySocket, [{active, once}]),
            Transport:send(ReplySocket, "ok"),
            ok;
        true ->
            ok
    end,
    {noreply, State};
handle_info({response, Translated}, State=#state{
                                             socket=Socket,
                                             transport=Transport}) ->
    error_logger:info_msg("~p ~p has a tcp response ~p", [?MODULE, self(), Translated]),
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, Translated),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("~p ~p is tcp_closed", [?MODULE, self()]),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    error_logger:info_msg("~p ~p is tcp_error for reason ~p", [?MODULE, self(), Reason]),
    {stop, Reason, State};
handle_info(timeout, State) ->
    error_logger:info_msg("~p ~p is timeout", [?MODULE, self()]),
    {stop, normal, State};
handle_info(Info, State) ->
    error_logger:info_msg("~p ~p got unknown info msg ~p", [?MODULE, self(), Info]),
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_to_target(<<>>) ->
    no;
send_to_target(B) when is_binary(B) ->
    {Device, CmdT} = case binary:split(B, <<"/">>, [global]) of
        [<<>>, _T, Dev, Cmd] ->
            {binary_to_atom(Dev, latin1), {Cmd, ""}};
        [<<>>, _T, Dev, Cmd, Val] ->
            {binary_to_atom(Dev, latin1), {Cmd, Val}};
        [<<>>, _T, Dev, Cmd, <<>>] ->
            {binary_to_atom(Dev, latin1), {Cmd, ""}};
        [<<>>, _T, Dev, Cmd, Val, <<>>] ->
            {binary_to_atom(Dev, latin1), {Cmd, Val}}
    end,
    ShouldWait = device:query_should_wait(Device, CmdT),
    device:translate_command(self(), Device, CmdT),
    error_logger:info_msg("~p ~p Sent to target ~p", [?MODULE, self(), B]),
    ShouldWait.
