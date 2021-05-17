-module(hank_test_utils).

-export([init_per_testcase/2, end_per_testcase/1]).
-export([init/0, init/1, mock_context/2, analyze_and_sort/2, analyze_and_sort/3,
         set_cwd/1, abs_test_path/1]).

init_per_testcase(Config, TestDirName) ->
    {ok, Cwd} = file:get_cwd(), % Keep the original cwd
    set_cwd(TestDirName),
    [{cwd, Cwd} | Config].

end_per_testcase(Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    ok = file:set_cwd(Cwd),
    NewConfig.

%% @doc Initialize rebar3 to simulate running `rebar3 hank`
init() ->
    {ok, State} =
        rebar3_hank:init(
            rebar_state:new()),
    State.

init(AppName) when is_atom(AppName) ->
    RebarAppInfo =
        rebar_app_info:name(
            rebar_app_info:new(), atom_to_binary(AppName, utf8)),
    RebarAppInfo2 = rebar_app_info:dir(RebarAppInfo, abs_test_path(atom_to_list(AppName))),
    rebar_state:project_apps(init(), RebarAppInfo2).

mock_context(Apps, ProjectApps) ->
    AppsAbs = maps:map(fun(_App, Path) -> filename:absname(Path) end, Apps),
    hank_context:new(AppsAbs, ProjectApps).

analyze_and_sort(Files, Rules) ->
    analyze_and_sort(Files, Rules, mock_context(#{}, [])).

analyze_and_sort(Files, Rules, Context) when is_map(Context) ->
    analyze_and_sort(Files, [], Rules, Context);
analyze_and_sort(Files, IgnoreSpecs, Rules) ->
    analyze_and_sort(Files, IgnoreSpecs, Rules, mock_context(#{}, [])).

analyze_and_sort(Files, IgnoreSpecs, Rules, Context) ->
    #{stats := Stats, results := Results} = hank:analyze(Files, IgnoreSpecs, Rules, Context),
    #{parsing := Parsing,
      analyzing := Analyzing,
      total := Total} =
        Stats,
    {true, Stats} = {Parsing >= 0, Stats},
    {true, Stats} = {Analyzing >= 0, Stats},
    {true, Stats} = {Parsing + Analyzing =< Total, Stats},
    lists:sort(Results).

set_cwd(RelativePathOrFilename) ->
    ok = file:set_cwd(abs_test_path(RelativePathOrFilename)).

abs_test_path(FilePath) ->
    R = filename:join(
            code:lib_dir(rebar3_hank), "test/files/" ++ FilePath),
    ct:pal("~p", [R]),
    R.
