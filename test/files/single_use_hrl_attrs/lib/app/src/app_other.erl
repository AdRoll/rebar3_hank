-module(app_other).

-include("ignore.hrl").
-include("header1.hrl").
-include("header3.hrl").

-record(yet_another_record, {another_record :: #another_record{}}).

-export([my_function/0]).

my_function() ->
    R = #a_record{used_field = used_field, used_typed_field = used_typed_field},
    U = #'unicode_αåβö'{'attr_αåβö' = R},
    _ = ?SOME_MACRO_3(U),
    ?APP_HEADER_3 ++ ?SOME_DEFINE.
