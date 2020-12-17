-module(used_once_hrls_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

all() ->
    [].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "used_once_hrls").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).
