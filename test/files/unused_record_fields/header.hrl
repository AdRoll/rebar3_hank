%% This file contains a record with all the fields that are used or unused
%% in unused_record_field_sample.erl
%%
%% Even when the module doesn't include this header, Hank will only report the
%% strictly unused record fields as unused. It will act conservatively and not
%% make any effort to actually verify where each record field that's used is
%% defined.
%% That's too hard. And if you have a project with multiple definitions of the
%% same record with the same fields... well... as long as one of them is used,
%% none of them will be reported. Sorry.
-record(a_record,
        {used_field,
         used_typed_field :: used_typed_field,
         used_field_with_default = used_field_with_default,
         used_typed_field_with_default = used_typed_field_with_default ::
             used_typed_field_with_default,
         unused_field,
         unused_typed_field :: unused_typed_field,
         unused_field_with_default = unused_field_with_default,
         unused_typed_field_with_default = unused_typed_field_with_default ::
             unused_typed_field_with_default}).
-record(really_unused_record, {really_unused_field}).
