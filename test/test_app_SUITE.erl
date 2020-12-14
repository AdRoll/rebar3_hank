-module(test_app_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1, without_warnings/1]).

%% @todo Include with_warnings/1 once we have rules to emit them
all() ->
    [with_warnings, without_warnings].

init_per_testcase(_, Config) ->
    {ok, Cwd} = file:get_cwd(),
    [{cwd, Cwd} | Config].

end_per_testcase(_, Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    file:set_cwd(Cwd),
    NewConfig.

%% @doc In a project where there are things to report, hank should return error
with_warnings(_Config) ->
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/with_warnings")),

    %% Initialize rebar3 state as if we run `rebar3 hank`
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),

    %% Alter the equivalent to rebar.config's hank to use a global rejector rule
    State1 = rebar_state:set(State, hank, [{rules, [hank_reject_every_file_rule]}]),

    %% Run hank
    {error, Error} = rebar3_hank_prv:do(State1),
    <<"The following pieces of code",
      " are dead and should be removed:\n",
      ResultsBin/binary>> =
        iolist_to_binary(Error),

    Results = binary:split(ResultsBin, <<$\n>>, [global, trim]),

    %% There are at least 8 files in the with_warnings folder.
    %% We might add more in the future and we don't want this test to fail
    %% just because of that.
    true = 8 =< length(Results),
    lists:foreach(fun(Result) ->
                     %% each result looks like path/to/file:#: msg
                     %% msg may include the character :, too
                     true = 3 =< length(binary:split(Result, <<$:>>, [global, trim]))
                  end,
                  Results).

%% @doc In a project where all rules run cleanly, hank should return OK
without_warnings(_Config) ->
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/without_warnings")),

    %% Initialize rebar3 state as if we run `rebar3 hank` with the default rules
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),

    %% Run hank
    {ok, _} = rebar3_hank_prv:do(State).
