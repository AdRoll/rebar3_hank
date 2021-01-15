-module(unused_ignored_function_params_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1]).

all() ->
    [with_warnings, without_warnings].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_ignored_function_params").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused function params
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused function params"),

    FileA = "warnings_A.erl",
    FileB = "warnings_B.erl",
    [#{file := FileA,
       line := 6,
       text := <<"Param #2 is not used at 'single_fun/2'">>},
     #{file := FileA,
       line := 10,
       text := <<"Param #3 is not used at 'multi_fun/3'">>},
     #{file := FileA,
       line := 18,
       text := <<"Param #1 is not used at 'unicode_αβåö/1'"/utf8>>},
     #{file := FileB,
       line := 6,
       text := <<"Param #1 is not used at 'underscore/3'">>}] =
        analyze([FileA, FileB]),
    ok.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the file is clean from warnings"),
    [] = analyze(["clean.erl", "gen_server_imp.erl"]),
    ok.

analyze(Files) ->
    hank_test_utils:analyze_and_sort(Files, [unused_ignored_function_params]).
