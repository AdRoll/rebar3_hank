%%% @doc Tests for the unused_macros rule
-module(unused_macros_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused_macros/1]).

all() ->
    [unused_macros].

init_per_testcase(_, Config) ->
    {ok, Cwd} = file:get_cwd(),
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/unused_macros")),
    [{cwd, Cwd} | Config].

end_per_testcase(_, Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    file:set_cwd(Cwd),
    NewConfig.

%% @doc Hank finds unused record files
unused_macros(_) ->
    Files = filelib:wildcard("*.erl"),
    [#{file := "unused_macro_sample.erl",
       line := 5,
       text := <<"?UNUSED_MACRO is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 6,
       text := <<"?UNUSED_MACRO_WITH/0 is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 7,
       text := <<"?UNUSED_MACRO_WITH/1 is unused">>},
     #{file := "unused_macro_sample.erl",
       line := 8,
       text := <<"?UNUSED_MACRO_WITH/2 is unused">>}] =
        lists:sort(
            hank:analyze(Files, [unused_macros], hank_context:new(#{}))).
