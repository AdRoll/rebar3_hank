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
    [#{file := "lib/app/include/header1.hrl",
       text := <<"?APP_HEADER_1 is used only at lib/app/src/app_include_lib.erl">>},
     #{file := "lib/app/include/header1.hrl",
       text := <<"?SOME_MACRO_1/1 is used only at lib/app/src/app_include_lib.erl">>},
     #{file := "lib/app/include/header1.hrl",
       text := <<"?SOME_DEFINE is used only at lib/app/src/app_other.erl">>},
     #{file := "lib/app/include/header2.hrl",
       text := <<"?APP_HEADER_2 is used only at lib/app/src/app_include.erl">>},
     #{file := "lib/app/include/header2.hrl",
       text := <<"?SOME_MACRO_2/1 is used only at lib/app/src/app_include.erl">>},
     #{file := "lib/app/include/header3.hrl",
       text := <<"#a_record is used only at lib/app/src/app_other.erl">>},
     #{file := "lib/app/include/header3.hrl",
       text := <<"#another_record is used only at lib/app/src/app_other.erl">>},
     #{file := "lib/app/include/header3.hrl",
       text := <<"#'unicode_αåβö' is used only at lib/app/src/app_other.erl"/utf8>>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_0 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_0/1 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_0/2 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_1 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_1/0 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_1/2 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_NONE/0 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_NONE/1 is used only at lib/app/src/ignore.erl">>},
     #{file := "lib/app/include/ignore.hrl",
       text := <<"?MACRO_NONE/2 is used only at lib/app/src/ignore.erl">>}] =
        analyze(Files),
    ok.

analyze(Files) ->
    Apps = #{app0 => "lib/app"},
    Context = hank_test_utils:mock_context(Apps, [app0]),
    analyze(Files, Context).

analyze(Files, Context) ->
    hank_test_utils:analyze_and_sort(Files, [single_use_hrl_attrs], Context).
