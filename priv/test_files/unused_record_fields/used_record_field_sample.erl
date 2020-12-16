-module(used_record_field_sample).

-compile(export_all).

-record(a_record,
        {used_field,
         used_typed_field :: used_typed_field,
         used_field_with_default = used_field_with_default,
         used_typed_field_with_default = used_typed_field_with_default ::
             used_typed_field_with_default}).

%% This doesn't count as usage
-type a_type() ::
    #a_record{}.

%% This doesn't count as usage
construct() ->
    #a_record{used_field = used_field,
              used_typed_field = used_typed_field}.

%% This doesn't count as usage either
update(R) ->
    R#a_record{used_field = used_field,
               used_typed_field = used_typed_field}.

%% This counts as usage
pattern_match(#a_record{used_field = UF} = R) ->
    #a_record{used_field_with_default = UFWD} = R,
    [UF, UFWD].

%% This counts as usage
index(Rs) ->
    lists:keysort(#a_record.used_typed_field, Rs).

%% This counts as usage, too
retrieve(R) ->
    R#a_record.used_typed_field_with_default.
