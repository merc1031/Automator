-module(xbmc_event_client).

-export([get_specification/0]).
-export([should_wait/1]).

-export([packet_helo/3, packet_action/2, packet_notification/4, packet_ping/0]).
-export([packet_get_header/2, packet_get_payload/2, packet_get_udp_message/2, packet_get_payload_size/2]).

-define(NOW, now()).

-define(UNIQUE_IDENTIFICATION, element(1, ?NOW) * 1000000 + element(2, ?NOW)).

-record(xbmc_packet, {
          sig = <<"XBMC">> :: binary(),
          minver = 0 :: non_neg_integer(),
          majver = 2 :: non_neg_integer(),
          seq = 1 :: non_neg_integer(),
          maxseq = 1 :: non_neg_integer(),
          payloadsize = 0 :: non_neg_integer(),
          uid = ?UNIQUE_IDENTIFICATION :: time:timestamp(),
          reserved = << <<"\0">> || _ <- lists:seq(0,9) >> :: binary(),
          payload = <<"">> :: binary(),
          packettype = <<"">> :: binary(),
          icontype = <<"">> :: binary()
}).

-define(MAX_PACKET_SIZE, 1024).
-define(MAX_HEADER_SIZE, 32).
-define(MAX_PAYLOAD_SIZE, (?MAX_PACKET_SIZE - ?MAX_HEADER_SIZE)).

-define(ICON_NONE, 16#00).

-define(PT_HELO, 16#01).
-define(PT_BYE,  16#02).
-define(PT_BUTTON, 16#03).
-define(PT_MOUSE,  16#04).
-define(PT_PING,   16#05).
-define(PT_BROADCAST, 16#06).
-define(PT_NOTIFICATION, 16#07).
-define(PT_BLOB, 16#08).
-define(PT_LOG,  16#09).
-define(PT_ACTION, 16#0A).
-define(PT_DEBUG,  16#FF).

-define(ACTION_EXECBUILTIN, = 16#01).
-define(ACTION_BUTTON, = 16#02).

-define(BT_USE_NAME, 16#01).
-define(BT_DOWN,  16#02).
-define(BT_UP, 16#04).
-define(BT_USE_AMOUNT,  16#08).
-define(BT_QUEUE,   16#10).
-define(BT_NO_REPEAT, 16#20).
-define(BT_VKEY, 16#40).
-define(BT_AXIS, 16#80).
-define(BT_AXISSINGLE,  16#100).


get_specification() ->
    Name = {name, xbmc_event_client},
    CommandMap = {command_map, #{
                    <<"right">> => {multi, packet_send_str(packet_button(#{map_name => <<"KB">>, button_name => <<"right">>, queue => 1, repeat => 0}))},
                    <<"left">> => {multi, packet_send_str(packet_button(#{map_name => <<"KB">>, button_name => <<"left">>, queue => 1, repeat => 0}))},
                    <<"up">> => {multi, packet_send_str(packet_button(#{map_name => <<"KB">>, button_name => <<"up">>, queue => 1, repeat => 0}))},
                    <<"down">> => {multi, packet_send_str(packet_button(#{map_name => <<"KB">>, button_name => <<"down">>, queue => 1, repeat => 0}))},
                    <<"enter">> => {multi, packet_send_str(packet_button(#{map_name => <<"KB">>, button_name => <<"enter">>, queue => 1, repeat => 0}))},
                    <<"release">> => {multi, packet_send_str(packet_button(#{code => 16#01, down => 0, queue => 0}))},
                    <<"notify">> => fun(_Cmd, Val) -> {multi, packet_send_str(packet_notification(Val, Val, ?ICON_NONE, undefined))} end
    }},

    InitialDataState = {initial_data_state, #{
    }},
    Target = {target, {
                udp_bridge,
                register_device,
                [
                 "192.168.1.129",
                 9777,
                 #{
                   init => packet_send_str(packet_helo(<<"automator">>, ?ICON_NONE, undefined)),
                   keepalive => {20000, packet_send_str(packet_ping())}
                  }
                ]
               }
             },
    Params = [Name, CommandMap, Target, InitialDataState],
    Params.

should_wait(_) ->
    no.

%%Packet utils

format_string(String) when is_binary(String) ->
    <<String/binary, <<"\x00">>/binary >>;
format_string(String) ->
    format_string(<<String>>).

format_uint(Uint, Bits) ->
    <<Uint:Bits/integer>>.

format_uint16(Uint) ->
    format_uint(Uint, 16).

format_uint32(Uint) ->
    format_uint(Uint, 32).

file(File) ->
    case file:read_file(File) of
        {error, Reason} ->
            error_logger:error_msg("Could not open file ~p for reading because ~p", [File, Reason]),
            <<"">>;
        {ok, Binary} ->
            Binary
    end.

chr(Uint) when Uint =< 0; Uint < 256 ->
    <<Uint:8>>.

ord(<<A:1/binary,_/binary>>=O) when is_binary(O) ->
    A;
ord([A]=O) when is_list(O) ->
    A.

%%Packet construction
packet_base() ->
    #xbmc_packet{}.

packet_helo(Name, IconType, IconFile) ->
    P = packet_base(),
    PacketType = ?PT_HELO,
    FormattedName = format_string(Name),
    %%Payload Layout
    %%
    %%string::128 Name
    %%char IconTyp
    %%uint16 reserved
    %%uint32 reserved
    %%uint32 reserved
    %%file? IconFile
    %%
    P2 = packet_set_payload(binary:part(FormattedName, 0, min(byte_size(FormattedName), 128)), P#xbmc_packet{packettype=PacketType, icontype=IconType}),
    P3 = lists:foldl(fun(X, Acc) -> packet_append_payload(X, Acc) end, P2, [chr(IconType), format_uint16(0), format_uint32(0), format_uint32(0)]),
    case {IconType, IconFile} of
        {?ICON_NONE, _} ->
            P3;
        {_, undefined} ->
            P3;
        {_, IconFileI } ->
            packet_append_payload(file(IconFileI), P3)
    end.

packet_action(Message, Action) ->
    P = packet_base(),
    PacketType = ?PT_ACTION,
    %%Payload Layout
    %%
    %%char Action
    %%string Message
    %%
    P2 = packet_append_payload(chr(Action), P#xbmc_packet{packettype=PacketType}),
    packet_append_payload(format_string(Message), P2).

packet_button(Args) ->
    DefaultArgs = #{
      code => 0,
      repeat => 1,
      down => 1,
      queue => 0,
      map_name => <<"">>,
      button_name => <<"">>,
      amount => 0,
      axis => 0
     },
    FinalArgs = maps:merge(DefaultArgs, Args),

    packet_button(
      maps:get(code, FinalArgs),
      maps:get(repeat, FinalArgs),
      maps:get(down, FinalArgs),
      maps:get(queue, FinalArgs),
      maps:get(map_name, FinalArgs),
      maps:get(button_name, FinalArgs),
      maps:get(amount, FinalArgs),
      maps:get(axis, FinalArgs)
     ).

packet_button(Code, Repeat, Down, Queue, MapName, ButtonName, Amount, Axis) ->
    P = packet_base(),
    PacketType = ?PT_BUTTON,
    Flags = 0,

    CodeU = case Code of Code when is_list(Code) -> ord(Code); _ -> Code end,
    {NewCode, NewFlags} = case {MapName, ButtonName} of
        {<<"">>, <<"">>} ->
            {CodeU, Flags};
        {_, _} ->
            IFlags = Flags bor ?BT_USE_NAME,
            {0, IFlags}
    end,

    {NewAmount, NewFlags2} = case Amount of
        undefined ->
            {0, NewFlags};
        Amount ->
            IFlags2 = NewFlags bor ?BT_USE_AMOUNT,
            {Amount, IFlags2}
    end,

    Opts = [{Down, {?BT_UP, ?BT_DOWN}}, {Repeat, {?BT_NO_REPEAT, 0}}, {Queue, {0, ?BT_QUEUE}}, {Axis, {0, ?BT_AXISSINGLE, ?BT_AXIS}}],
    Applicator = fun({Flag, Applics}, Acc) ->
                         Acc bor element(Flag + 1, Applics)
                 end,

    FinalFlags = lists:foldl(Applicator, NewFlags2, Opts),

    %%Payload Layout
    %%
    %%uint16 Code
    %%uint16 Flags
    %%uint16 Amount
    %%string MapName
    %%string ButtonName
    %%
    P2 = packet_set_payload(format_uint16(NewCode), P#xbmc_packet{packettype=PacketType}),
    lists:foldl(fun(X, Acc) -> packet_append_payload(X, Acc) end, P2, [format_uint16(FinalFlags), format_uint16(NewAmount), format_string(MapName), format_string(ButtonName)]).

packet_notification(Title, Message, IconType, IconFile) ->
    P = packet_base(),
    PacketType = ?PT_NOTIFICATION,
    %%Payload Layout
    %%
    %%string Title
    %%string Message
    %%char IconType
    %%uint32 reserved
    %%file? IconFile
    %%
    P2 = packet_set_payload(format_string(Title), P#xbmc_packet{packettype=PacketType}),
    P3 = lists:foldl(fun(X, Acc) -> packet_append_payload(X, Acc) end, P2, [format_string(Message), chr(IconType), format_uint32(0)]),
    case {IconType, IconFile} of
        {?ICON_NONE, _} ->
            P3;
        {_, undefined} ->
            P3;
        {_, IconFileI } ->
            packet_append_payload(file(IconFileI), P3)
    end.

packet_ping() ->
    P = packet_base(),
    P#xbmc_packet{packettype=?PT_PING}.

%%Packet functions
packet_get_header(1, #xbmc_packet{packettype=PacketType}=P) ->
    packet_get_header(PacketType, 1, P);
packet_get_header(PacketNum, #xbmc_packet{}=P) ->
    packet_get_header(?PT_BLOB, PacketNum, P).

packet_get_header(HeaderType, PacketNum, #xbmc_packet{
                                            sig=Sig,
                                            majver=MajVer,
                                            minver=MinVer,
                                            maxseq=MaxSeq,
                                            uid=Uid,
                                            reserved=Reserved
                                           }=P) ->
    MajVerBin = chr(MajVer),
    MinVerBin = chr(MinVer),
    HeaderBin = format_uint16(HeaderType),
    PacketNumBin = format_uint32(PacketNum),
    MaxSeqBin = format_uint32(MaxSeq),
    PayloadSizeBin = format_uint16(packet_get_payload_size(PacketNum, P)),
    UidBin = format_uint32(Uid),
    <<
    Sig:4/binary,
    MajVerBin:1/binary,
    MinVerBin:1/binary,
    HeaderBin:2/binary,
    PacketNumBin:4/binary,
    MaxSeqBin:4/binary,
    PayloadSizeBin:2/binary,
    UidBin:4/binary,
    Reserved:10/binary
    >>.

packet_get_payload(PacketNum, #xbmc_packet{payload=Payload}=P) ->
    Start = (PacketNum - 1) * ?MAX_PAYLOAD_SIZE,
    End = (PacketNum - 1) * ?MAX_PAYLOAD_SIZE + packet_get_payload_size(PacketNum, P),
    binary:part(Payload, Start, End).

packet_num_packets(#xbmc_packet{maxseq=MaxSeq}) ->
    MaxSeq.

packet_append_payload(Payload, #xbmc_packet{payload=PacketPayload}=P) ->
    packet_set_payload(<<PacketPayload/binary,Payload/binary>>, P).

packet_set_payload(Payload, #xbmc_packet{}=P) ->
    PayloadSize = byte_size(Payload),
    MaxSeq = trunc((PayloadSize + (?MAX_PAYLOAD_SIZE - 1)) / ?MAX_PAYLOAD_SIZE),
    P#xbmc_packet{maxseq=MaxSeq, payloadsize=PayloadSize, payload=Payload}.

packet_get_payload_size(_, #xbmc_packet{maxseq=1, payloadsize=PayloadSize}) ->
    PayloadSize;
packet_get_payload_size(PacketNum, #xbmc_packet{maxseq=MaxSeq}) when PacketNum < MaxSeq ->
    ?MAX_PAYLOAD_SIZE;
packet_get_payload_size(_, #xbmc_packet{payloadsize=PayloadSize}) ->
    PayloadSize rem ?MAX_PACKET_SIZE.

packet_get_udp_message(Index, #xbmc_packet{}=P) ->
    Header = packet_get_header(Index, P),

    Payload = packet_get_payload(Index, P),
    <<Header:32/binary, Payload/binary>>.

packet_send_str(#xbmc_packet{}=P) ->
    lists:foldl(fun(Ind, Acc) ->
            [packet_get_udp_message(Ind+1, P) | Acc]
        end,
        [],
        lists:seq(0, packet_num_packets(P) - 1)).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

proper_helo_packet_test() ->
    Expected = <<"XBMC\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x15S\x94\xfcM\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00Automator\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00">>,
    ExpectedHeader = <<"XBMC\x02\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x15S\x94\xfcM\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00">>,
    HELOPacket = packet_helo(<<"Automator">>, 16#00, undefined),
    MungedHELOPacket = HELOPacket#xbmc_packet{uid=1402272845},

    Header = packet_get_header(1, MungedHELOPacket),

    ExpectedHeader = Header,
    Data = packet_get_udp_message(1, MungedHELOPacket),
    Expected = Data.

proper_button_packet_test() ->
    Expected = <<"XBMC\x02\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x01\x00\x0fS\x94\xfcM\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00KB\x00right\x00">>,
    ExpectedHeader = <<"XBMC\x02\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x01\x00\x0fS\x94\xfcM\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00">>,
    ButtonPacket = packet_button(#{ map_name => <<"KB">>, button_name => <<"right">>}),
    MungedButtonPacket = ButtonPacket#xbmc_packet{uid=1402272845},

    Header = packet_get_header(1, MungedButtonPacket),

    ExpectedHeader = Header,
    Data = packet_get_udp_message(1, MungedButtonPacket),
    Expected = Data.

proper_non_repeating_button_packet_test() ->
    Expected = <<"XBMC\x02\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x01\x00\x0fS\x94\xfcM\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00;\x00\x00KB\x00right\x00">>,
    ButtonPacket = packet_button(#{map_name => <<"KB">>, button_name => <<"right">>, repeat => 0, queue => 1}),
    MungedButtonPacket = ButtonPacket#xbmc_packet{uid=1402272845},

    Data = packet_get_udp_message(1, MungedButtonPacket),
    Expected = Data.
-endif.
