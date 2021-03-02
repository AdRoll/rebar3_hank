-module(unnecessary_function_arguments_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1, macros/1, ignore/1]).

all() ->
    [with_warnings, without_warnings, macros, ignore].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unnecessary_function_arguments").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused function params
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused function params"),

    FileA = "warnings_A.erl",
    FileB = "warnings_B.erl",
    [#{file := FileA,
       line := 6,
       text := <<"single_fun/2 doesn't need its #2 argument">>},
     #{file := FileA,
       line := 10,
       text := <<"multi_fun/3 doesn't need its #3 argument">>},
     #{file := FileA,
       line := 18,
       text := <<"unicode_αβåö/1 doesn't need its #1 argument"/utf8>>},
     #{file := FileA,
       line := 21,
       text := <<"with_nif_stub/2 doesn't need its #1 argument">>},
     #{file := FileB,
       line := 6,
       text := <<"underscore/3 doesn't need its #1 argument">>}] =
        analyze([FileA, FileB]),
    ok.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the files are clean from warnings"),
    [] = analyze(["clean.erl", "gen_server_imp.erl", "nifs.erl"]),
    ok.

%% @doc Macros as function names should work
macros(_Config) ->
    ct:comment("Macros as function names should not crash hank"),
    [#{file := "macros.erl",
       line := 4,
       text := <<"?MODULE/1 doesn't need its #1 argument">>}] =
        analyze(["macros.erl"]),
    ok.

%% @doc Hank should correctly ignore warnings
ignore(_Config) ->
    ct:comment("Should correctly ignore warnings"),
    [#{file := "ignore.erl",
       line := 16,
       text := <<"ignore_arg2/2 doesn't need its #2 argument">>}] =
        analyze(["ignore.erl"]),
    ok.

analyze(Files) ->
    hank_test_utils:analyze_and_sort(Files, [unnecessary_function_arguments]).
