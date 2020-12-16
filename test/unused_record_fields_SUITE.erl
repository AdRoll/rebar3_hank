%%% @doc Tests for the unused_record_fields rule
-module(unused_record_fields_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused_record_fields/1]).

all() ->
    [unused_record_fields].

init_per_testcase(_, Config) ->
    {ok, Cwd} = file:get_cwd(),
    ok =
        file:set_cwd(
            filename:join(
                code:priv_dir(rebar3_hank), "test_files/unused_record_fields")),
    [{cwd, Cwd} | Config].

end_per_testcase(_, Config) ->
    {value, {cwd, Cwd}, NewConfig} = lists:keytake(cwd, 1, Config),
    file:set_cwd(Cwd),
    NewConfig.

%% @doc Hank finds unused record files
unused_record_fields(_) ->
    Files = filelib:wildcard("*.erl"),
    [#{file := "unused_record_field_sample.erl",
       line := 11,
       text := <<"Field unused_field in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       line := 12,
       text := <<"Field unused_typed_field in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       line := 13,
       text := <<"Field unused_field_with_default in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       line := 14,
       text :=
           <<"Field unused_typed_field_with_default in record a_record is "
             "unused">>}] =
        lists:sort(
            hank:analyze(Files, [unused_record_fields], hank_context:new(#{}))).
