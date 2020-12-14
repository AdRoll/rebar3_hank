-module(test_rules_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([with_warnings/1]).

%% @todo Include with_warnings/1 once we have rules to emit them
all() ->
    [with_warnings].

init_per_testcase(_, Config) ->
    {ok, Cwd} = file:get_cwd(),
    [{cwd, Cwd} | Config].

end_per_testcase(_, Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    file:set_cwd(Cwd),
    NewConfig.

%% @doc In a project where there are things to report, hank should return error
with_warnings(_Config) ->
    Files =
        [filename:join(
             code:priv_dir(rebar3_hank), "test_files/with_warnings/src/a_module.erl")],
    ASTs =
        lists:foldl(fun(File, Acc) ->
                       {ok, AST} = ktn_dodger:parse_file(File, [{scan_opts, [text]}, no_fail]),
                       [{File, AST} | Acc]
                    end,
                    [],
                    Files),
    Result = hank_rule:analyze(unused_function_params, ASTs),
    ok.
