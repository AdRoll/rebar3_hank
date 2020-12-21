-module(app_include).

-include("header2.hrl").
-include("header3.hrl").

-export([my_function/0]).

my_function() ->
  % those are only used here!
  Val = ?SOME_MACRO_2(?APP_HEADER_2),
  % and those are used in other module :)
  Val ++ ?SOME_MACRO_3(?APP_HEADER_3).
