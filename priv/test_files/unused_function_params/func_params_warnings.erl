-module(func_params_warnings).

-export([single_fun/2, underscore_fun/4, multi_fun/2]).

%% _Arg2 is unused
single_fun(Arg1, _Arg2) ->
  Arg1.

%% _ is unused
underscore_fun(Arg1, _, Arg3, Arg4) ->
  {Arg1, Arg3, Arg4}.

%% A multi-clause function with unused 2nd param
multi_fun(undefined, _) ->
  ok;
multi_fun(Arg1, _Arg2) when is_binary(Arg1) ->
  ok;
multi_fun(Arg1, _) ->
  Arg1.