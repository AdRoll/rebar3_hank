-module(unused_record_field_sample).

-compile(export_all).

-record(a_record, {
    used_field,
    used_typed_field :: used_typed_field,
    used_field_with_default = used_field_with_default,
    used_typed_field_with_default = used_typed_field_with_default ::
        used_typed_field_with_default,
    unused_field,
    unused_typed_field :: unused_typed_field,
    unused_field_with_default = unused_field_with_default,
    unused_typed_field_with_default = unused_typed_field_with_default ::
        unused_typed_field_with_default
}).
-doc """
Unicode should be supported and not break!.
""".
-record('unicode_αåβö', {'attr_αåβö' :: a_type()}).

-doc """
This doesn't count as usage.
""".
-type a_type() ::
    #a_record{
        unused_field :: unused_field,
        unused_field_with_default :: unused_field_with_default
    }.

-doc """
This doesn't count as usage.
""".
construct() ->
    #a_record{used_field = used_field, used_typed_field = used_typed_field}.

-doc """
This doesn't count as usage either.
""".
update(R) ->
    R#a_record{used_field = used_field, used_typed_field = used_typed_field}.

-doc """
This counts as usage.
""".
pattern_match(#a_record{used_field = UF}, R) ->
    #a_record{used_field_with_default = UFWD} = R,
    [UF, UFWD].

-doc """
This counts as usage.
""".
index(Rs) ->
    lists:keysort(#a_record.used_typed_field, Rs).

-doc """
This counts as usage, too.
""".
retrieve(R) ->
    R#a_record.used_typed_field_with_default.
