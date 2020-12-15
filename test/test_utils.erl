-module(test_utils).

-export([init_per_testcase/2,
         end_per_testcase/1,
         mock_context/1,
         hank_analyze_and_sort/2,
         hank_analyze_and_sort/3,
         hank_set_cwd/1,
         hank_abs_test_path/1]).

init_per_testcase(Config, TestDirName) ->
    {ok, Cwd} = file:get_cwd(), % Keep the original cwd
    hank_set_cwd(TestDirName),
    [{cwd, Cwd} | Config].

end_per_testcase(Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    ok = file:set_cwd(Cwd),
    NewConfig.

mock_context(Apps) ->
  AppsAbs = maps:map(fun(_App, Path) ->
    filename:absname(Path)
  end, Apps),
  hank_context:new(AppsAbs).

hank_analyze_and_sort(Files, Rules) ->
  hank_analyze_and_sort(Files, Rules, mock_context(#{})).
hank_analyze_and_sort(Files, Rules, Context) ->
  lists:sort(hank:analyze(Files, Rules, Context)).

hank_set_cwd(RelativePathOrFilename) ->
  ok = file:set_cwd(hank_abs_test_path(RelativePathOrFilename)).

hank_abs_test_path(FilePath) ->
  filename:join(code:priv_dir(rebar3_hank), "test_files/"++FilePath).