%%% @doc Test module for the hidden files/folders functionality
-module(hidden_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([hidden/1]).

all() ->
    [hidden].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "hidden").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc No warning should be emitted for files that are hidden or that are
%%      included in hidden folders.
hidden(_Config) ->
    State = hank_test_utils:init(),

    {ok, _} = rebar3_hank_prv:do(State),

    {comment, ""}.
