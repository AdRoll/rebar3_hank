%%% @doc Tests for the unused ignore warnings
-module(unused_ignores_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused_ignores/1]).

all() ->
    [unused_ignores].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_ignores").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused record files
unused_ignores(_) ->
    Files = filelib:wildcard("*.?rl"),
    IgnoreSpecs = [{"ignore_config.erl", unused_macros, ["MACRO_FROM_CONFIG"]}],
    #{results := [],
      unused_ignores :=
          [{"ignore_all.erl", unused_macros, all},
           {"ignore_config.erl", unused_macros, ["MACRO_FROM_CONFIG"]},
           {"unused_ignores.erl", bad_rule, all},
           {"unused_ignores.erl",
            unnecessary_function_arguments,
            [{non_exported_function_with, 2, 1}]},
           {"unused_ignores.erl", unused_macros, ["MACRO", "MICRO"]}]} =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unused_macros]).
