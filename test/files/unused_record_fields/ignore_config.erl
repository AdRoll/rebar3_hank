-module ignore.

-compile(export_all).

%% @doc No warnings should be emitted for this file since
%%      rebar.config specifically states that all of them
%%      should be ignored.

-record(a_record, {used_field, ignored_field1, ignored_field2}).
-record(ignored_record, {used_field, unused_field, ignored_field_1, ignored_field_2}).

usage() ->
    #a_record{used_field = #ignored_record{used_field = used}}.
