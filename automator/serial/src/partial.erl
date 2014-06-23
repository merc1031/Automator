-module(partial).
-export([make/3, call/2]).

-include("partial.hrl").

make(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    try
        {F, _Arity} = proplists:lookup(Function, Module:module_info(exports))
    catch
        error:Error ->
            error_logger:error_msg("Function ~p is not exported in Module ~p", [Function, Module]),
            erlang:error(Error)
    end,
    #partial{module=Module, function=Function, args=Args}.

call(Fn=#partial{}, Args) when is_list(Args) ->
    erlang:apply(Fn#partial.module, Fn#partial.function, Fn#partial.args ++ Args).
