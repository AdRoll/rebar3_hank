-module(test_app_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1]).

%% @todo Include with_warnings/1 once we have rules to emit them
all() ->
    [with_warnings, without_warnings].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "test_app").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc In a project where there are things to report, hank should return error
with_warnings(_Config) ->
    %% Initialize rebar3 state as if we run `rebar3 hank`
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),

    ct:comment("With default rules, there should be warnings since "
               ++ "hank_rule:default_rules() should find global_rejector"),
    State1 = rebar_state:set(State, hank, []),
    find_warnings(State1),

    ct:comment("If we alter the equivalent to rebar.config's hank to use a "
               "global rejector rule, it should find the same warnings"),
    State2 = rebar_state:set(State, hank, [{rules, [global_rejector]}]),
    find_warnings(State2),

    {comment, ""}.

%% @doc In a project where all rules run cleanly, hank should return OK
without_warnings(_Config) ->
    %% Initialize rebar3 state as if we run `rebar3 hank` with the default rules
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),

    ct:comment("With no rules, there should be no warnings"),
    State2 = rebar_state:set(State, hank, [{rules, []}]),
    {ok, _} = rebar3_hank_prv:do(State2),

    ct:comment("Without the global rejector, there should be no warnings either"),
    Rules = hank_rule:default_rules() -- [global_rejector],
    State3 = rebar_state:set(State, hank, [{rules, Rules}]),
    {ok, _} = rebar3_hank_prv:do(State3),

    {comment, ""}.

find_warnings(State) ->
    %% Run hank
    {error, Error} = rebar3_hank_prv:do(State),
    <<"The following pieces of code",
      " are dead and should be removed:\n",
      ResultsBin/binary>> =
        iolist_to_binary(Error),
    Results = binary:split(ResultsBin, <<$\n>>, [global, trim]),

    %% There are at least 8 files in the test_app folder.
    %% We might add more in the future and we don't want this test to fail
    %% just because of that.
    true = 8 =< length(Results),
    lists:foreach(fun(Result) ->
                     %% each result looks like path/to/file:#: msg
                     [_, <<"1">>, <<" global_rejector">>] =
                         binary:split(Result, <<$:>>, [global, trim])
                  end,
                  Results).
