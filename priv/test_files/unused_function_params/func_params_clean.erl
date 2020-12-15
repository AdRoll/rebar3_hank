-module(func_params_clean).

-export([single_fun/2, multi_fun/3]).

single_fun(Arg1, Arg2) ->
  Arg1 * Arg2.

multi_fun(_Arg1, Arg2, Arg3) when Arg2 > 1 ->
  [Arg2, Arg3];
multi_fun(Arg1, _, Arg3) ->
  [Arg1, Arg3].
