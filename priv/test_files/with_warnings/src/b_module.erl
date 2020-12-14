-module(b_module).

-export([my_fun/2]).

%% _Arg2 is unused
my_fun(Arg1, Arg2) ->
  a_module:my_fun(Arg2, Arg1).