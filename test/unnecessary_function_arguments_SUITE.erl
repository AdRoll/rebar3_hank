-module(unnecessary_function_arguments_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
    with_warnings/1,
    without_warnings/1,
    macros/1,
    ignore/1,
    ignore_config/1,
    ct_suite/1,
    export_all/1,
    header_behaviour/1,
    header_behaviour_include_lib/1,
    header_behaviour_transitive/1,
    header_behaviour_otp/1,
    header_behaviour_cycle/1,
    header_behaviour_not_included/1,
    header_behaviour_macro_include/1,
    header_behaviour_not_analyzed/1,
    header_behavior_us_spelling/1,
    header_behaviour_same_name/1,
    header_behaviour_ifdef/1,
    header_behaviour_and_own/1,
    header_behaviour_odd_include_lib/1,
    header_behaviour_cycle_entry_points/1,
    header_behaviour_longer_cycles/1
]).

all() ->
    [
        with_warnings,
        without_warnings,
        macros,
        ignore,
        ct_suite,
        export_all,
        header_behaviour,
        header_behaviour_include_lib,
        header_behaviour_transitive,
        header_behaviour_otp,
        header_behaviour_cycle,
        header_behaviour_not_included,
        header_behaviour_macro_include,
        header_behaviour_not_analyzed,
        header_behavior_us_spelling,
        header_behaviour_same_name,
        header_behaviour_ifdef,
        header_behaviour_and_own,
        header_behaviour_odd_include_lib,
        header_behaviour_cycle_entry_points,
        header_behaviour_longer_cycles
    ].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unnecessary_function_arguments").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

-doc """
Hank finds unused function parameters.
""".
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused function parameters"),

    FileA = "warnings_A.erl",
    FileB = "warnings_B.erl",
    FileC = "a_behaviour.erl",
    FileD = "a_behaviour_imp.erl",
    FileE = "gen_server_imp.erl",
    [
        #{
            file := FileC,
            text := <<"a_function_from_the_behaviour/1 doesn't need its #1 argument">>
        },
        #{file := FileD, text := <<"non_exported_function_with/2 doesn't need its #1 argument">>},
        #{file := FileE, text := <<"my_function/1 doesn't need its #1 argument">>},
        #{file := FileE, text := <<"my_other_function/2 doesn't need its #2 argument">>},
        #{file := FileA, text := <<"single_fun/2 doesn't need its #2 argument">>},
        #{file := FileA, text := <<"multi_fun/3 doesn't need its #3 argument">>},
        #{file := FileA, text := <<"unicode_αβåö/1 doesn't need its #1 argument"/utf8>>},
        #{file := FileA, text := <<"with_nif_stub/2 doesn't need its #1 argument">>},
        #{file := FileB, text := <<"underscore/3 doesn't need its #1 argument">>}
    ] =
        analyze([FileA, FileB, FileC, FileD, FileE, "macro_behaviour_imp.erl"]),
    ok.

-doc """
Hank finds nothing!
""".
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the files are clean from warnings"),
    [] =
        analyze([
            "weird.erl",
            "clean.erl",
            "nifs.erl",
            "macro_behaviour_imp.erl",
            "parse_transf.erl"
        ]),
    ok.

-doc """
Macros as function names should work.
""".
macros(_Config) ->
    ct:comment("Macros as function names should not crash hank"),
    [#{file := "macros.erl", text := <<"?MODULE/1 doesn't need its #1 argument">>}] =
        analyze(["macros.erl"]),
    ok.

-doc """
Hank should correctly ignore warnings.
""".
ignore(_Config) ->
    ct:comment("Should correctly ignore warnings"),
    [#{file := "ignore.erl", text := <<"ignore_arg2/2 doesn't need its #2 argument">>}] =
        analyze(["ignore.erl"]),
    ok.

-doc """
No warnings since rebar.config specifically states that all of them should be ignored.
""".
ignore_config(_) ->
    File = "ignore_config.erl",
    Files = [File],
    IgnoreSpecs =
        [
            {File, unnecessary_function_arguments, [
                {ignore_arg2, 3, 2},
                {ignore_arg2, 2, 1},
                {ignore_arg2, 2, 2},
                {ignore_whole_func3, 3},
                ignore_whole_func
            ]}
        ],
    #{results := [], unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unnecessary_function_arguments]).

-doc """
Common Test suites should be ignored.
""".
ct_suite(_Config) ->
    ct:comment("CT suites should not generate warnings"),
    Files =
        case code:which(ct_suite) of
            % OTP < 23.2
            non_existing ->
                ["old_ct_SUITE.erl"];
            _ ->
                ["ct_SUITE.erl"]
        end,
    [] = analyze(Files).

-doc """
Compiler flag `export_all` should be supported.
""".
export_all(_Config) ->
    [] = analyze(["export_all1.erl"]),
    [] = analyze(["export_all2.erl"]).

-doc """
A behaviour declared in an included header file is implemented by the module.
""".
header_behaviour(_Config) ->
    [
        #{
            file := "header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["behaviour_in_header.hrl", "header_behaviour_imp.erl"]),
    ok.

-doc """
A behaviour declared in a header file included with `-include_lib` is implemented by the module.
""".
header_behaviour_include_lib(_Config) ->
    [
        #{
            file := "lib_header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["include/lib_behaviour.hrl", "lib_header_behaviour_imp.erl"]),
    ct:comment("The app folder doesn't match the app name"),
    Imp = "lib/renamed-app/src/renamed_header_behaviour_imp.erl",
    Context = hank_test_utils:mock_context(#{renamed_app => "lib/renamed-app"}, [renamed_app]),
    #{
        results := [
            #{
                file := Imp,
                text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
            }
        ],
        unused_ignores := []
    } =
        hank_test_utils:analyze_and_sort(
            ["lib/renamed-app/include/renamed_behaviour.hrl", Imp],
            [unnecessary_function_arguments],
            Context
        ),
    ok.

-doc """
A behaviour declared in a header file included by an included header file is implemented
by the module.
""".
header_behaviour_transitive(_Config) ->
    [
        #{
            file := "transitive_header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["outer_header.hrl", "inner_header.hrl", "transitive_header_behaviour_imp.erl"]),
    ok.

-doc """
An OTP behaviour declared in an included header file only exempts its callbacks.
""".
header_behaviour_otp(_Config) ->
    [
        #{
            file := "header_gen_server_imp.erl",
            text := <<"my_function/1 doesn't need its #1 argument">>
        }
    ] =
        analyze(["gen_server_in_header.hrl", "header_gen_server_imp.erl"]),
    ok.

-doc """
Include cycles between header files are not a problem.
""".
header_behaviour_cycle(_Config) ->
    [
        #{
            file := "cycle_header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["cycle_a.hrl", "cycle_b.hrl", "cycle_header_behaviour_imp.erl"]),
    ok.

-doc """
A behaviour declared in a header file that the module doesn't include is not implemented
by the module.
""".
header_behaviour_not_included(_Config) ->
    [
        #{
            file := "no_header_behaviour_imp.erl",
            text := <<"a_kind_of_magic/1 doesn't need its #1 argument">>
        }
    ] =
        analyze([
            "behaviour_in_header.hrl",
            "outer_header_without_behaviour.hrl",
            "no_header_behaviour_imp.erl"
        ]),
    ok.

-doc """
If the path of an included header file is a macro, any analyzed header file may be the one
declaring the behaviour.
""".
header_behaviour_macro_include(_Config) ->
    [] = analyze(["behaviour_in_header.hrl", "macro_header_behaviour_imp.erl"]),
    ok.

-doc """
A behaviour declared in a header file that is not analyzed is not implemented by the module,
since Hank can't see it.
""".
header_behaviour_not_analyzed(_Config) ->
    [
        #{
            file := "header_behaviour_imp.erl",
            text := <<"a_kind_of_magic/1 doesn't need its #1 argument">>
        },
        #{
            file := "header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["header_behaviour_imp.erl"]),
    ok.

-doc """
A behaviour declared with the US spelling of the attribute in an included header file is
implemented by the module.
""".
header_behavior_us_spelling(_Config) ->
    [
        #{
            file := "header_behavior_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["behavior_in_header.hrl", "header_behavior_imp.erl"]),
    ok.

-doc """
If two analyzed header files share their name, Hank can't tell which one an `-include` refers
to, so a behaviour declared in either of them counts as implemented by the module.
""".
header_behaviour_same_name(_Config) ->
    Imp = "lib/app_b/src/same_name_header_imp.erl",
    [] =
        analyze(["lib/app_a/include/shared.hrl", "lib/app_b/include/shared.hrl", Imp]),
    ct:comment("Without the header file that declares the behaviour, the function is warned"),
    [#{file := Imp, text := <<"a_kind_of_magic/1 doesn't need its #1 argument">>}] =
        analyze(["lib/app_b/include/shared.hrl", Imp]),
    ok.

-doc """
A behaviour declared in a conditional block of an included header file counts as implemented
by the module, since Hank doesn't evaluate the condition.
""".
header_behaviour_ifdef(_Config) ->
    [
        #{
            file := "ifdef_header_behaviour_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["ifdef_behaviour_in_header.hrl", "ifdef_header_behaviour_imp.erl"]),
    ok.

-doc """
The callbacks of a behaviour declared in the module and the ones of a behaviour declared in an
included header file are all exempt.
""".
header_behaviour_and_own(_Config) ->
    [
        #{
            file := "own_and_header_behaviour_imp.erl",
            text := <<"my_function/1 doesn't need its #1 argument">>
        }
    ] =
        analyze(["gen_server_in_header.hrl", "own_and_header_behaviour_imp.erl"]),
    ok.

-doc """
`-include_lib` paths that can't refer to any project app (an empty path or one whose first
component is too long to be an app name) don't prevent Hank from analyzing the module.
""".
header_behaviour_odd_include_lib(_Config) ->
    [
        #{
            file := "odd_include_lib_imp.erl",
            text := <<"non_exported_function_with/2 doesn't need its #1 argument">>
        }
    ] =
        analyze(["behaviour_in_header.hrl", "odd_include_lib_imp.erl"]),
    ok.

-doc """
In an include cycle, the behaviour declared in one of the header files is implemented by the
modules that include any of them, no matter which one of the files is analyzed first.
""".
header_behaviour_cycle_entry_points(_Config) ->
    Warning = <<"non_exported_function_with/2 doesn't need its #1 argument">>,
    Headers = ["ring2_a.hrl", "ring2_b.hrl"],
    ImpA = "ring2_a_imp.erl",
    ImpB = "ring2_b_imp.erl",
    Expected = [{ImpA, Warning}, {ImpB, Warning}],
    [
        Expected = warnings(Files)
     || Files <- [Headers ++ [ImpA, ImpB], Headers ++ [ImpB, ImpA], [ImpB, ImpA | Headers]]
    ],
    ct:comment("A module including the header file without the behaviour, on its own"),
    [
        [{ImpB, Warning}] = warnings(Files)
     || Files <- [Headers ++ [ImpB], lists:reverse(Headers) ++ [ImpB]]
    ],
    ok.

-doc """
In a cycle of three header files, the behaviour declared in the first one is implemented by a
module that includes the last one, by a module that includes the second one (two includes away
from the behaviour), and also by a module that includes a header file that includes itself and
the first one.
""".
header_behaviour_longer_cycles(_Config) ->
    Warning = <<"non_exported_function_with/2 doesn't need its #1 argument">>,
    Ring = ["ring3_a.hrl", "ring3_b.hrl", "ring3_c.hrl"],
    Imp = "ring3_c_imp.erl",
    [
        [{Imp, Warning}] = warnings(Files)
     || Files <- [Ring ++ [Imp], lists:reverse(Ring) ++ [Imp], [Imp | lists:reverse(Ring)]]
    ],
    ct:comment("The behaviour two includes away"),
    ImpB = "ring3_b_imp.erl",
    [
        [{ImpB, Warning}] = warnings(Files)
     || Files <- [Ring ++ [ImpB], lists:reverse(Ring) ++ [ImpB], [ImpB | Ring]]
    ],
    ct:comment("A header file that includes itself"),
    SelfImp = "self_loop_imp.erl",
    [
        [{SelfImp, Warning}] = warnings(Files)
     || Files <- [
            ["self_loop.hrl" | Ring] ++ [SelfImp],
            lists:reverse(Ring) ++ ["self_loop.hrl", SelfImp]
        ]
    ],
    ok.

warnings(Files) ->
    [{File, Text} || #{file := File, text := Text} <- analyze(Files)].

analyze(Files) ->
    #{results := Results, unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, [unnecessary_function_arguments]),
    Results.
