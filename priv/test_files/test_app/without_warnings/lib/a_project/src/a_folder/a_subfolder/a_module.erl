-module(a_module).

-include("a_header.hrl").
-include("a_folder/a_header.hrl").

-define(A_MACRO_WITH_AN_OPTION, application:get_env(my_config_from_app_src)).

-export([a_function/0]).
a_function() ->
  [?A_MACRO_WITH_AN_OPTION,
  application:get_env(test_app, environment, default_value)].
