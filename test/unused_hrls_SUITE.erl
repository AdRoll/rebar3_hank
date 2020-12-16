%%% @doc Tests for the unused_hrls rule
-module(unused_hrls_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused/1, local_include/1, remote_include/1, local_include_lib/1,
         remote_include_lib/1, versioned_include_lib/1]).

all() ->
    [unused,
     local_include,
     remote_include,
     local_include_lib,
     remote_include_lib,
     versioned_include_lib].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_hrls").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused header files
unused(_) ->
    ct:comment("If there are no Erlang files, all hrls should be unused."),
    OnlyHrls = ["lib/app0-with-other-name/include/header.hrl", "lib/app1/include/header.hrl"],
    [#{file := "lib/app0-with-other-name/include/header.hrl",
       line := 0,
       text := "This file is unused"},
     #{file := "lib/app1/include/header.hrl",
       line := 0,
       text := "This file is unused"}] =
        analyze(OnlyHrls),

    ct:comment("If there are Erlang files that don't include the hrls, all "
               "hrls should be unused."),
    WithErls =
        ["lib/app0-with-other-name/include/header.hrl",
         "lib/app1/include/header.hrl",
         "lib/app1/src/app1_not_using_header.erl",
         "lib/app2/src/app2_not_using_header.erl"],
    [#{file := "lib/app0-with-other-name/include/header.hrl"},
     #{file := "lib/app1/include/header.hrl"}] =
        analyze(WithErls),

    {comment, ""}.

%% @doc Hank detects that a header file is used with an include
local_include(_) ->
    ct:comment("lib/app1/include/header.hrl should not be marked as unused "
               "since it is used locally"),
    Apps1And0 =
        ["lib/app1/include/header.hrl",
         "lib/app1/src/app1_not_using_header.erl",
         "lib/app1/src/app1_include.erl"],
    [] = analyze(Apps1And0),

    ct:comment("include/header.hrl should not be marked as unused since it "
               "is used locally"),
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/unused_hrls/lib/app1")),
    OnlyApp1 =
        ["include/header.hrl", "src/app1_not_using_header.erl", "src/app1_include.erl"],
    [] = analyze(OnlyApp1),

    {comment, ""}.

remote_include(_) ->
    ct:comment("lib/app1/include/header.hrl should not be marked as unused "
               "since it is used from app2"),
    Apps1And0 =
        ["lib/app0-with-other-name/include/header.hrl",
         "lib/app1/include/header.hrl",
         "lib/app2/src/app2_not_using_header.erl",
         "lib/app2/src/app2_include.erl"],
    [#{file := "lib/app0-with-other-name/include/header.hrl",
       line := 0,
       text := "This file is unused"}] =
        analyze(Apps1And0),

    {comment, ""}.

local_include_lib(_) ->
    ct:comment("include/header.hrl should not be marked as unused since it "
               "is used in app1_include_lib"),
    Apps =
        #{app0 => "lib/app0-with-other-name",
          app1 => "lib/app1",
          app2 => "lib/app2"},
    Context = hank_test_utils:mock_context(Apps),
    hank_test_utils:set_cwd("unused_hrls/lib/app1"),
    OnlyApp1 =
        ["include/header.hrl", "src/app1_not_using_header.erl", "src/app1_include_lib.erl"],
    [] = analyze(OnlyApp1, Context),

    {comment, ""}.

remote_include_lib(_) ->
    ct:comment("No header file should be marked as unused since they're both "
               "used in app2_include_lib"),
    Apps1And0 =
        ["lib/app0-with-other-name/include/header.hrl",
         "lib/app1/include/header.hrl",
         "lib/app2/src/app2_not_using_header.erl",
         "lib/app2/src/app2_include_lib.erl"],
    [] = analyze(Apps1And0),

    {comment, ""}.

versioned_include_lib(_) ->
    ct:comment("No header file should be marked as unused since they're both "
               "used in app1_include_lib"),
    Apps1And0 =
        ["lib/app0-with-other-name/include/header.hrl",
         "lib/app1/include/header.hrl",
         "lib/app1/src/app1_not_using_header.erl",
         "lib/app1/src/app1_include_lib.erl"],
    [] = analyze(Apps1And0),

    {comment, ""}.

analyze(Files) ->
    Apps =
        #{app0 => "lib/app0-with-other-name",
          app1 => "lib/app1",
          app2 => "lib/app2"},
    Context = hank_test_utils:mock_context(Apps),
    analyze(Files, Context).

analyze(Files, Context) ->
    hank_test_utils:analyze_and_sort(Files, [unused_hrls], Context).
