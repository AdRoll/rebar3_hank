-module(app_include_lib).

-include_lib("app/include/header1.hrl").

-export([my_function/0]).

my_function() ->
  % those are only used here!
  ?SOME_MACRO_1(?APP_HEADER_1).
