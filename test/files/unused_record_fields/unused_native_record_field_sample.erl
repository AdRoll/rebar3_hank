-module(unused_native_record_field_sample).
-moduledoc """
This module just proves that `unused_record_fields` **ignores** native records.
""".

-compile(export_all).

-if(?OTP_RELEASE >= 29).

% erlfmt-ignore-begin

-record #a_record{
    used_field,
    used_typed_field :: used_typed_field,
    used_field_with_default = used_field_with_default,
    used_typed_field_with_default = used_typed_field_with_default :: used_typed_field_with_default,
    unused_field_with_default = unused_field_with_default,
    unused_typed_field_with_default = unused_typed_field_with_default :: unused_typed_field_with_default
}.

-record #'unicode_αåβö'{'attr_αåβö' :: a_type()}.

% erlfmt-ignore-end

-doc """
This doesn't count as usage.
""".
-type a_type() :: a_type.

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
    lists:keysort(Rs#a_record.used_typed_field, Rs).

-doc """
This counts as usage, too.
""".
retrieve(R) ->
    R#a_record.used_typed_field_with_default.

-endif.
