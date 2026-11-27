-module(non_regular_SUITE).
-moduledoc """
Test module for glob matches that are not regular files.
""".

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([directory_match/1]).

all() ->
    [directory_match].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "non_regular").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

-doc """
A directory whose name matches the file glob should be skipped.
""".
directory_match(_Config) ->
    State = hank_test_utils:init(),
    State1 = rebar_state:set(State, hank, [{rules, []}, {parsing_style, sequential}]),
    {ok, _} = rebar3_hank_prv:do(State1),
    {comment, ""}.
