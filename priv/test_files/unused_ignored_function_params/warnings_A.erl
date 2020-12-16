-module(warnings_A).

-export([single_fun/2, multi_fun/3]).

%% _Arg2 is unused
single_fun(Arg1, _Arg2) ->
  Arg1.

%% A multi-clause function with unused 3rd param
multi_fun(undefined, _, _) ->
  ok;
multi_fun(Arg1, Arg2, _Arg3) when is_binary(Arg1) ->
  Arg2;
multi_fun(Arg1, _, _) ->
  Arg1.
