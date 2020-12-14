-module(a_module).

-export([my_fun/2, multi_fun/2]).

%% _Arg2 is unused
my_fun(Arg1, _Arg2) ->
  Arg1.

%% A multi-clause function with Arg2 being unused
multi_fun(undefined, _Arg2) ->
  ok;
multi_fun(Arg1, _Arg2) when is_binary(Arg1) ->
  ok;
multi_fun(Arg1, _Arg2) ->
  Arg1.