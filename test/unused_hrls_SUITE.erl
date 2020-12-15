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

init_per_testcase(local_include_lib, Config) ->
    meck:new(hank_utils, [passthrough]),
    % We need this because code:lib_dir/1 fails at this stage. It won't fail on
    % a _real_ project because we'll be running within a rebar3-prepared
    % environment and it will have a lib.
    meck:expect(hank_utils,
                expand_lib_dir,
                fun ("app0/" ++ Rest, _) ->
                        filename:absname("lib/app0-with-other-name/" ++ Rest);
                    ("app1/" ++ Rest, _) ->
                        filename:absname("lib/app1/" ++ Rest);
                    (Name, _) ->
                        meck:passthrough([Name])
                end),
    init_per_testcase(default, Config);
init_per_testcase(_, Config) ->
    {ok, Cwd} = file:get_cwd(),
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/unused_hrls")),
    [{cwd, Cwd} | Config].

end_per_testcase(local_include_lib, Config) ->
    meck:unload(hank_utils),
    Config;
end_per_testcase(_, Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    file:set_cwd(Cwd),
    NewConfig.

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
    ct:comment("No header file should be marked as unused since they're both "
               "used in app1_include_lib"),
    Apps1And0 =
        ["lib/app0-with-other-name/include/header.hrl",
         "lib/app1/include/header.hrl",
         "lib/app1/src/app1_not_using_header.erl",
         "lib/app1/src/app1_include_lib.erl"],
    [] = analyze(Apps1And0),

    ct:comment("include/header.hrl should not be marked as unused since it "
               "is used in app1_include_lib"),
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/unused_hrls/lib/app1")),
    OnlyApp1 =
        ["include/header.hrl", "src/app1_not_using_header.erl", "src/app1_include_lib.erl"],
    [] = analyze(OnlyApp1),

    {comment, ""}.

remote_include_lib(_) ->
    ct:fail("Not Implemented").

versioned_include_lib(_) ->
    ct:fail("Not Implemented").

analyze(Apps) ->
    lists:sort(
        hank:analyze(Apps, [unused_hrls], hank_context:empty())).
