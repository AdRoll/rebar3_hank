-module(unused_function_params_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1]).

all() ->
    [with_warnings, without_warnings].

init_per_testcase(_, Config) ->
    test_utils:init_per_testcase(Config, "unused_function_params").

end_per_testcase(_, Config) ->
    test_utils:end_per_testcase(Config).

%% @doc Hank finds unused function params
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused function params"),

    File = "func_params_warnings.erl",
    %% @TODO Complete the expectations
    Expected = [#{file => test_utils:hank_abs_test_path(File),
                  line => 0,
                  text => "WIP"}],
    Expected = analyze(File),
    ok.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the file is clean from warnings"),
    [] = analyze("func_params_clean.erl"),
    ok.

analyze(File) ->
    test_utils:hank_analyze_and_sort([File], [unused_function_params]).