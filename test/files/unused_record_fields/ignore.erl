-module ignore.

-compile(export_all).

-hank([{unused_record_fields,
        [ignored_record, {a_record, ignored_field_1}, {a_record, ignored_field_2}]}]).

-record(a_record, {used_field, unused_field, ignored_field_1, ignored_field_2}).
-record(ignored_record, {used_field, unused_field, ignored_field_1, ignored_field_2}).

usage() ->
    #a_record{used_field = #ignored_record{used_field = used}}.
