-module(specific_ignore).

-hank([unused_macros,
       {unnecessary_function_arguments, [{no_ignore, 2}, {do_ignore, 1, 1}, do_ignore_me_too]}]).

-define(UNUSED_MACRO, unused_macro).

-export([no_ignore/2, do_ignore/1, do_ignore_me_too/1, do_ignore_me_too/2,
         do_ignore_me_too/3]).

no_ignore(_, _) ->
    no_ignore.

do_ignore(_Arg1) ->
    ok.

do_ignore_me_too(_Arg1) ->
    ok.

do_ignore_me_too(_Arg1, _Arg2) ->
    ok.

do_ignore_me_too(_Arg1, _Arg2, _Arg3) ->
    ok.
