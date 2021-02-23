-module(clean).

-define(FC(X, Y), {X, Y}).
-record(r, {fc = function_call}).

-export([single_fun/2, multi_fun/3]).
-export([function_call/0, function_call/1, function_call/2, function_call/3])

single_fun(Arg1, Arg2) ->
    Arg1 * Arg2.

multi_fun(_Arg1, Arg2, Arg3) when Arg2 > 1 ->
    [Arg2, Arg3];
multi_fun(Arg1, _, Arg3) ->
    [Arg1, Arg3].

function_call() ->
    function_call(not_a_nif).

function_call(UsedArg1) ->
    F = function_call,
    F(UsedArg1, two).

function_call(UsedArg1, UsedArg2) ->
    ?FC(UsedArg1, UsedArg2).

function_call(UsedArg1, UsedArg2, UsedArg3) ->
    ?MODULE:function_call(UsedArg1),
    (UsedArg2#r.fc)(UsedArg2),
    begin function_call end(UsedArg3).
