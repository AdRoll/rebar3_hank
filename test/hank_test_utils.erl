-module(hank_test_utils).

-export([init_per_testcase/2, end_per_testcase/1, mock_context/1, analyze_and_sort/2,
         analyze_and_sort/3, set_cwd/1, abs_test_path/1]).

init_per_testcase(Config, TestDirName) ->
    {ok, Cwd} = file:get_cwd(), % Keep the original cwd
    set_cwd(TestDirName),
    [{cwd, Cwd} | Config].

end_per_testcase(Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    ok = file:set_cwd(Cwd),
    NewConfig.

mock_context(Apps) ->
    AppsAbs = maps:map(fun(_App, Path) -> filename:absname(Path) end, Apps),
    hank_context:new(AppsAbs).

analyze_and_sort(Files, Rules) ->
    analyze_and_sort(Files, Rules, mock_context(#{})).

analyze_and_sort(Files, Rules, Context) ->
    lists:sort(
        maps:get(results, hank:analyze(Files, [], Rules, Context))).

set_cwd(RelativePathOrFilename) ->
    ok = file:set_cwd(abs_test_path(RelativePathOrFilename)).

abs_test_path(FilePath) ->
    filename:join(
        code:priv_dir(rebar3_hank), "test_files/" ++ FilePath).
