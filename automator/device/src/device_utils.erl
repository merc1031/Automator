-module(device_utils).

-export([line_protocol_helper/3, with_cached_value/3, binary_join/2, run_regex/2]).

-spec line_protocol_helper(A, A, A) -> {list(A), A} when A :: binary().
line_protocol_helper(NewData, OldData, Delim) ->
    Total = <<OldData/binary, NewData/binary>>,
    Acc = fun AccumDataAndParse(Remaining, {Ret, <<>>}) ->
        case binary:split(Remaining, Delim) of
            [Data] -> % no delim found
                {lists:reverse(Ret), Data};
            [Line, Rest] ->
                AccumDataAndParse(Rest, {[Line | Ret], <<>>})
        end
    end,
    Acc(Total, {[],<<>>}).

with_cached_value(Key, DataState, Operation) ->
    case maps:find(Key, DataState) of %%Lookup Raw Stored
        {ok, CachedVal} ->
            Operation(CachedVal);
        error ->
            ""
    end.

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun (A, B) ->
                        if
                            bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                            true -> A
                        end
                end, <<>>, List).

-spec run_regex(list(binary()), re:mp()) -> list(list(binary())).
run_regex([<<>>],_) ->
    [[<<>>]];
run_regex(Working, Parser) ->
    Matches = lists:foldl(fun(Elem, Acc) ->
                                          {match, Match} = re:run(Elem, Parser, [global, {capture, all_but_first, binary}]),
                                          lists:append(Acc, Match)
                                 end, [], Working),
    Matches.

