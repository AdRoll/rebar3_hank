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
    Files = filelib:wildcard("*.?rl"),
    IgnoreSpecs =
        [{"ignore_config.erl",
          unused_macros,
          ["MACRO_ALL", {"MACRO_0", 0}, {"MACRO_1", 1}, {"MACRO_NONE", none}]}],
    #{results :=
          [#{file := "double.erl", text := <<"?Y is unused">>},
           #{file := "double.erl", text := <<"?Y is unused">>},
           #{file := "header.hrl", text := <<"?UNUSED_MACRO is unused">>},
           #{file := "header.hrl", text := <<"?UNUSED_MACRO_WITH/0 is unused">>},
           #{file := "header.hrl", text := <<"?UNUSED_MACRO_WITH/1 is unused">>},
           #{file := "header.hrl", text := <<"?UNUSED_MACRO_WITH/2 is unused">>},
           #{file := "header.hrl", text := <<"?UNUSED_MACRO_UNICODE_ÇØÍ is unused"/utf8>>},
           #{file := "ignore.erl", text := <<"?MACRO_0 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_0/1 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_0/2 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_1 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_1/0 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_1/2 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_NONE/0 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_NONE/1 is unused">>},
           #{file := "ignore.erl", text := <<"?MACRO_NONE/2 is unused">>},
           #{file := "unused_macro_sample.erl", text := <<"?UNUSED_MACRO is unused">>},
           #{file := "unused_macro_sample.erl", text := <<"?UNUSED_MACRO_WITH/0 is unused">>},
           #{file := "unused_macro_sample.erl", text := <<"?UNUSED_MACRO_WITH/1 is unused">>},
           #{file := "unused_macro_sample.erl", text := <<"?UNUSED_MACRO_WITH/2 is unused">>},
           #{file := "unused_macro_sample.erl",
             text := <<"?UNUSED_MACRO_UNICODE_ÇØÍ is unused"/utf8>>},
           #{file := "unused_macro_sample.erl", text := <<"?macroIsAnAtom is unused">>}],
      unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unused_macros]).
