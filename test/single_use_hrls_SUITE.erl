-module(single_use_hrls_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([only_hrls/1, single_use/1, respects_ignore/1, alltogether/1]).

all() ->
    [only_hrls, single_use, respects_ignore, alltogether].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "single_use_hrls").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank doesn't find anything to report for header files only
only_hrls(_) ->
    ct:comment("If there are no Erlang files, it should not detect anything."),
    OnlyHrls = ["include/multi.hrl", "include/single.hrl"],
    [] = analyze(OnlyHrls),
    ok.

%% @doc Hank finds header files that are only included in just one module
single_use(_) ->
    ct:comment("It should detect include/single.hrl because it's only included "
               "at src/include_single.erl"),
    Files =
        ["include/multi.hrl",
         "include/single.hrl",
         "src/include_multi.erl",
         "src/include_single.erl"],
    [#{file := "include/single.hrl",
       line := 0,
       text := <<"This header file is only included at: src/include_single.erl">>}] =
        analyze(Files),
    ok.

%% @doc Hank respects the `ignore` attribute in a header file
respects_ignore(_) ->
    ct:comment("It should not detect include/ignored.hrl because is ignored "
               "although it's only included at src/include_ignored.erl"),
    Files =
        ["include/multi.hrl",
         "include/ignored.hrl",
         "src/include_multi.erl",
         "src/include_ignored.erl"],
    [] = analyze(Files),
    ok.

%% @doc Hank finds and ignores accordingly
alltogether(_) ->
    ct:comment("It should detect include/single.hrl because it's only included "
               "at src/include_single.erl and not detect include/ignored.hrl"),
    Files =
        ["include/multi.hrl",
         "include/single.hrl",
         "include/single_unicode.hrl",
         "include/ignored.hrl",
         "src/include_multi.erl",
         "src/include_single.erl",
         "src/include_ignored.erl"
         | filelib:wildcard("src/include_unicode_*.erl")],
    [#{file := "include/single.hrl",
       line := 0,
       text := <<"This header file is only included at: src/include_single.erl">>},
     #{file := "include/single_unicode.hrl",
       line := 0,
       text := <<"This header file is only included at: src/include_unicode_åö.erl"/utf8>>}] =
        analyze(Files),
    ok.

analyze(Files) ->
    hank_test_utils:analyze_and_sort(Files, [single_use_hrls]).
