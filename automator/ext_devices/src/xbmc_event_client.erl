-module(xbmc_event_client).

-export([get_specification/0]).

-record(xbmc_packet, {
          sig = "XBMC" :: string(),
          minver = 0 :: non_neg_integer(),
          majver = 2 :: non_neg_integer(),
          seq = 1 :: non_neg_integer(),
          maxseq = 1 :: non_neg_integer(),
          payloadsize = 0 :: non_neg_integer(),
          uid :: time:timestamp(),
          reserved = <<0000000000>> :: binary(),
          payload = "" :: string(),
          packettype = <<"">> :: binary(),
          icontype = <<"">> :: binary()
}).

-define(MAX_PACKET_SIZE, 1024).
-define(MAX_HEADER_SIZE, 32).
-define(MAX_PAYLOAD_SIZE, ?MAX_PACKET_SIZE - ?MAX_HEADER_SIZE).

-define(ICON_NONE, <<"\x00">>).

-define(PT_HELO, "0x01").
-define(PT_BYE,  "0x02").
-define(PT_BUTTON, "0x03").
-define(PT_MOUSE,  "0x04").
-define(PT_PING,   "0x05").
-define(PT_BROADCAST, "0x06").
-define(PT_NOTIFICATION, "0x07").
-define(PT_BLOB, "0x08").
-define(PT_LOG,  "0x09").
-define(PT_ACTION, "0x0A").
-define(PT_DEBUG,  "0xFF").

get_specification() ->
    Name = {name, xbmc_event_client},
    CommandMap = {command_map, #{
                    <<"right">> => packet_helo("automator", ?ICON_NONE, undefined)
    }},

    ResponseParser = {response_parser, {
                        undefined, undefined
    }},
    ResponseMap = {response_map, #{
    }},

    InitialDataState = {initial_data_state, #{
    }},
    Target = {target, {udp, "192.168.1.125", 9997}},
    CleanResponse = {clean_response_action,
                     undefined
    },
    Params = [Name, CommandMap, ResponseParser, ResponseMap, Target, InitialDataState, CleanResponse],
    Params.


%%Packet functions

format_string(String) when is_binary(String) ->
    <<String/binary, <<"\x00">>/binary >>;
format_string(String) ->
    format_string(<<String>>).

format_uint(Uint, Bits) ->
    <<Uint:Bits/binary>>.

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


packet_base() ->
    #xbmc_packet{}.

packet_helo(Name, IconType, IconFile) ->
    P = packet_base(),
    PacketType = ?PT_HELO,
    P2 = packet_set_payload(binary:part(format_string(Name), 0, 128), P#xbmc_packet{packettype=PacketType, icontype=IconType}),
    P3 = packet_append_payload(IconType, P2),
    P4 = packet_append_payload(format_uint16(0), P3),
    P5 = packet_append_payload(format_uint32(0), P4),
    P6 = packet_append_payload(format_uint32(0), P5),
    case {IconType, IconFile} of
        {?ICON_NONE, _} ->
            P6;
        {_, undefined} ->
            P6;
        {_, IconFileI } ->
            packet_append_payload(file(IconFileI), P6)
    end.

packet_get_header(#xbmc_packet{}=P) ->
    P.

packet_num_packets(#xbmc_packet{maxseq=MaxSeq}) ->
    MaxSeq.

packet_append_payload(Payload, #xbmc_packet{payload=PacketPayload}=P) ->
    packet_set_payload(PacketPayload ++ Payload, P).

packet_set_payload(Payload, #xbmc_packet{}=P) ->
    PayloadSize = length(Payload),
    MaxSeq = trunc((PayloadSize + (?MAX_PAYLOAD_SIZE - 1)) / ?MAX_PAYLOAD_SIZE),
    P#xbmc_packet{maxseq=MaxSeq, payloadsize=PayloadSize, payload=Payload}.

packet_get_payload_size(#xbmc_packet{}=P) ->
    P.

packet_get_udp_message(#xbmc_packet{}=P) ->
    P.

%packet_send(#xbmc_packet{}=P, Sender) ->
%    lists:foreach(fun(Ind) ->
%            Sender(packet_get_udp_message(Ind+1, P))
%        end,
%        lists:seq(0, packet_num_packets(P))).
packet_send_str(#xbmc_packet{}=P) ->
    lists:foldl(fun(Ind, Acc) ->
            [packet_get_udp_message(Ind+1, P) | Acc]
        end,
        lists:seq(0, packet_num_packets(P))).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).


-endif.
