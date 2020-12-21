%%% @doc Tests for the single_use_hrl_attrs rule
-module(single_use_hrl_attrs_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([hrl_in_just_one_module/1]).

all() ->
    [hrl_in_just_one_module].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "single_use_hrl_attrs").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds hrl macros used in just one module
%% Check msgs!
hrl_in_just_one_module(_) ->
    Files = filelib:wildcard("**/*.[he]rl"),
    [#{file := "lib/app/include/header3.hrl",
       line := 4,
       message := <<"?a_record is used only once at lib/app/src/app_other.erl">>},
     #{file := "lib/app/include/header1.hrl",
       line := 1,
       text := <<"?APP_HEADER_1 is used only once at lib/app/src/app_include_lib.erl">>},
     #{file := "lib/app/include/header1.hrl",
       line := 2,
       text := <<"?SOME_MACRO_1/1 is used only once at lib/app/src/app_include_lib.erl">>},
     #{file := "lib/app/include/header1.hrl",
       line := 3,
       text := <<"?SOME_DEFINE is used only once at lib/app/src/app_other.erl">>},
     #{file := "lib/app/include/header2.hrl",
       line := 1,
       text := <<"?APP_HEADER_2 is used only once at lib/app/src/app_include.erl">>},
     #{file := "lib/app/include/header2.hrl",
       line := 2,
       text := <<"?SOME_MACRO_2/1 is used only once at lib/app/src/app_include.erl">>}] =
        analyze(Files),

    ok.

analyze(Files) ->
    Apps = #{app0 => "lib/app"},
    Context = hank_test_utils:mock_context(Apps),
    analyze(Files, Context).

analyze(Files, Context) ->
    hank_test_utils:analyze_and_sort(Files, [single_use_hrl_attrs], Context).
