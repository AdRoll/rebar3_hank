-define(APP_HEADER_3, "this is header from app that will be used in different modules").
-define(SOME_MACRO_3(A), A).

-record(a_record,
        {used_field,
         used_typed_field :: used_typed_field}).

%% This doesn't count as usage
-type a_type() ::
    #a_record{}.