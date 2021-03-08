-module(unused_callbacks_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, with_macros/1, without_warnings/1]).

all() ->
    [with_warnings, with_macros, without_warnings].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_callbacks").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused callbacks
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused callbacks"),

    File = "warnings.erl",
    [#{file := File,
       line := 3,
       text := <<"Callback unused_callback/1 is not used anywhere in the module">>}] =
        analyze([File]),
    ok.

%% @doc Hank finds unused callbacks with macros
with_macros(_Config) ->
    ct:comment("Should detect and display warnings for unused callbacks with macros"),

    File = "macros.erl",
    [#{file := File,
       line := 4,
       text := <<"Callback unused_callback/0 is not used anywhere in the module">>}] =
        analyze([File]),
    ok.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the file is clean from warnings"),
    [] = analyze(["clean.erl"]),
    ok.

analyze(Files) ->
    hank_test_utils:analyze_and_sort(Files, [unused_callbacks]).
