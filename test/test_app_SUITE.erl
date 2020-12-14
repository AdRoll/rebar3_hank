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

with_warnings(_Config) ->
    ok = file:set_cwd("../../../../test/with_warnings"),
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),
    State1 = rebar_state:set(State, hank, [{rules, [hank_reject_every_file_rule]}]),
    {error, Error} = verify(State1),
    <<"The following pieces of code are dead and should be removed:\n", _/binary>> =
        iolist_to_binary(Error).

without_warnings(_Config) ->
    ok = file:set_cwd("../../../../test/without_warnings"),
    {ok, _} = verify().

verify() ->
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),
    verify(State).

verify(State) ->
    rebar3_hank_prv:do(State).
