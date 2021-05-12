%%% @doc Tests for the unused_record_fields rule
-module(unused_record_fields_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([unused_record_fields/1]).

all() ->
    [unused_record_fields].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "unused_record_fields").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc Hank finds unused record files
unused_record_fields(_) ->
    Files = filelib:wildcard("*.?rl"),
    IgnoreSpecs =
        [{"ignore_config.erl",
          unused_record_fields,
          [ignored_record, {a_record, ignored_field1}, {a_record, ignored_field2}]}],
    [#{file := "header.hrl",
       text := <<"Field really_unused_field in record really_unused_record is unused">>},
     #{file := "ignore.erl", text := <<"Field unused_field in record a_record is unused">>},
     #{file := "macros.erl", text := <<"Field unused_field in record a_rec is unused">>},
     #{file := "unused_record_field_sample.erl",
       text := <<"Field unused_field in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       text := <<"Field unused_typed_field in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       text := <<"Field unused_field_with_default in record a_record is unused">>},
     #{file := "unused_record_field_sample.erl",
       text :=
           <<"Field unused_typed_field_with_default in record a_record is "
             "unused">>},
     #{file := "unused_record_field_sample.erl",
       text := <<"Field 'attr_αåβö' in record 'unicode_αåβö' is unused"/utf8>>}] =
        hank_test_utils:analyze_and_sort(Files, IgnoreSpecs, [unused_record_fields]).
