-module(func_params_clean).

-export([single_fun/2, multi_fun/2]).

single_fun(Arg1, Arg2) ->
  Arg1 * Arg2.

multi_fun(_Arg1, Arg2) when Arg2 > 1 ->
  Arg2;
multi_fun(Arg1, _Arg2) ->
  Arg1.
