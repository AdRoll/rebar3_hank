-module(app_other).

-include("header1.hrl").
-include("header3.hrl").

-export([my_function/0]).

my_function() ->
  R = #a_record{used_field = used_field,
            used_typed_field = used_typed_field},
  _ = ?SOME_MACRO_3(R),
  ?APP_HEADER_3 ++ ?SOME_DEFINE.
