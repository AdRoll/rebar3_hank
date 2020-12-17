-module(single_use_hrls_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([only_hrls/1, single_use/1]).

all() ->
    [only_hrls, single_use].

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
               "at single.hrl"),
    ok.

analyze(Files) ->
    hank_test_utils:analyze_and_sort(Files, [single_use_hrls]).
