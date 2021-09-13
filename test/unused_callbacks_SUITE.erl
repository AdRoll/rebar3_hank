-module(unused_callbacks_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, with_macros/1, without_warnings/1, ignore/1, ignore_config/1]).

all() ->
    [with_warnings, with_macros, without_warnings, ignore, ignore_config].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_callbacks").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused callbacks
with_warnings(_Config) ->
    ct:comment("Should detect and display warnings for unused callbacks"),

    File = "warnings.erl",
    [#{file := File,
       text := <<"Callback unused_callback/1 is not used anywhere in the module">>}] =
        analyze([File]),
    {comment, ""}.

%% @doc Hank finds unused callbacks with macros
with_macros(_Config) ->
    ct:comment("Should detect and display warnings for unused callbacks with macros"),

    File = "macros.erl",
    [#{file := File,
       text := <<"Callback unused_callback/0 is not used anywhere in the module">>}] =
        analyze([File]),
    {comment, ""}.

%% @doc Hank finds nothing!
without_warnings(_Config) ->
    ct:comment("Should not detect anything since the file is clean from warnings"),
    [] = analyze(["clean.erl"]),
    {comment, ""}.

%% @doc Hank ignores some callbacks
ignore(_Config) ->
    ct:comment("Should only detect the callbacks that are not ignored"),
    [#{file := "ignore.erl",
       text := <<"Callback just_one/0 is not used anywhere in the module">>},
     #{file := "ignore.erl",
       text := <<"Callback just_one/2 is not used anywhere in the module">>}] =
        analyze(["ignore.erl"]),
    {comment, ""}.

%% @doc No warnings since rebar.config specifically states that all of them
%%      should be ignored.
ignore_config(_) ->
    File = "ignore_config.erl",
    Files = [File],
    IgnoreSpecs =
        [{File, unused_callbacks, [all_arities, {just_one, 0}, {just_one, 1}, {just_one, 2}]}],
    #{results := [], unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unused_callbacks]).

analyze(Files) ->
    #{results := Results, unused_ignores := []} =
        hank_test_utils:analyze_and_sort(Files, [unused_callbacks]),
    Results.
