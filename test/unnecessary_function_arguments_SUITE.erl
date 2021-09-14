-module(unnecessary_function_arguments_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1, macros/1, ignore/1, ignore_config/1,
         ct_suite/1]).

all() ->
    [with_warnings, without_warnings, macros, ignore, ct_suite].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unnecessary_function_arguments").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused function params
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused function params"),

    FileA = "warnings_A.erl",
    FileB = "warnings_B.erl",
    FileC = "a_behaviour.erl",
    FileD = "a_behaviour_imp.erl",
    FileE = "gen_server_imp.erl",
    [#{file := FileC,
       text := <<"a_function_from_the_behaviour/1 doesn't need its #1 argument">>},
     #{file := FileD, text := <<"non_exported_function_with/2 doesn't need its #1 argument">>},
     #{file := FileE, text := <<"my_function/1 doesn't need its #1 argument">>},
     #{file := FileE, text := <<"my_other_function/2 doesn't need its #2 argument">>},
     #{file := FileA, text := <<"single_fun/2 doesn't need its #2 argument">>},
     #{file := FileA, text := <<"multi_fun/3 doesn't need its #3 argument">>},
     #{file := FileA, text := <<"unicode_αβåö/1 doesn't need its #1 argument"/utf8>>},
     #{file := FileA, text := <<"with_nif_stub/2 doesn't need its #1 argument">>},
     #{file := FileB, text := <<"underscore/3 doesn't need its #1 argument">>}] =
        analyze([FileA, FileB, FileC, FileD, FileE, "macro_behaviour_imp.erl"]),
    ok.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the files are clean from warnings"),
    [] =
        analyze(["weird.erl",
                 "clean.erl",
                 "nifs.erl",
                 "macro_behaviour_imp.erl",
                 "parse_transf.erl"]),
    ok.

%% @doc Macros as function names should work
macros(_Config) ->
    ct:comment("Macros as function names should not crash hank"),
    [#{file := "macros.erl", text := <<"?MODULE/1 doesn't need its #1 argument">>}] =
        analyze(["macros.erl"]),
    ok.

%% @doc Hank should correctly ignore warnings
ignore(_Config) ->
    ct:comment("Should correctly ignore warnings"),
    [#{file := "ignore.erl", text := <<"ignore_arg2/2 doesn't need its #2 argument">>}] =
        analyze(["ignore.erl"]),
    ok.

%% @doc No warnings since rebar.config specifically states that all of them
%%      should be ignored.
ignore_config(_) ->
    File = "ignore_config.erl",
    Files = [File],
    IgnoreSpecs =
        [{File,
          unnecessary_function_arguments,
          [{ignore_arg2, 3, 2},
           {ignore_arg2, 2, 1},
           {ignore_arg2, 2, 2},
           {ignore_whole_func3, 3},
           ignore_whole_func]}],
    #{results := [], unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unnecessary_function_arguments]).

%% @doc Common Test suites should be ignored
ct_suite(_Config) ->
    ct:comment("CT suites should not generate warnings"),
    Files =
        case code:which(ct_suite) of
            non_existing -> % OTP < 23.2
                ["old_ct_SUITE.erl"];
            _ ->
                ["ct_SUITE.erl"]
        end,
    [] = analyze(Files).

analyze(Files) ->
    #{results := Results, unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, [unnecessary_function_arguments]),
    Results.
