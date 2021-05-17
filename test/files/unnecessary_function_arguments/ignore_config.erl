-module(ignore_config).

-export([ignore_arg2/3, ignore_arg2/2, ignore_whole_func3/3, ignore_whole_func/1,
         ignore_whole_func/2]).

%% Arg2 is unused but ignored
ignore_arg2(Arg1, _Arg2, Arg3) ->
    Arg1 + Arg3.

%% Arg1 and Arg2 are unused but just ignoring Arg1 for `ignore_arg2/2`
ignore_arg2(_Arg1, _Arg2) ->
    ok.

%% A multi-clause function with unused 1st param
ignore_whole_func3(_, _, undefined) ->
    ok;
ignore_whole_func3(_, Arg2, _) when is_binary(Arg2) ->
    Arg2;
ignore_whole_func3(_, _, Arg3) ->
    Arg3.

%% Same function names with different arities
ignore_whole_func(_) ->
    ok.

ignore_whole_func(_, _) ->
    ok.
