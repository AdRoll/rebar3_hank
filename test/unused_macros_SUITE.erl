%%% @doc Tests for the unused_macros rule
-module(unused_macros_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused_macros/1]).

all() ->
    [unused_macros].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_macros").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused record files
unused_macros(_) ->
    Files = filelib:wildcard("*.erl"),
    [#{file := "unused_macro_sample.erl",
       line := 5,
       text := <<"?UNUSED_MACRO is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 6,
       text := <<"?UNUSED_MACRO_WITH/0 is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 7,
       text := <<"?UNUSED_MACRO_WITH/1 is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 8,
       text := <<"?UNUSED_MACRO_WITH/2 is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 14,
       text := <<"?UNUSED_MACRO_UNICODE_ÇØÍ is unused"/utf8>>},
     #{file := "unused_macro_sample.erl",
       line := 16,
       text := <<"?macroIsAnAtom is unused">>}] =
        hank_test_utils:analyze_and_sort(Files, [unused_macros]).
