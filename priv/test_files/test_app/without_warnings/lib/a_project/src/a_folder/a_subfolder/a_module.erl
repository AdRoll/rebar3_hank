-module(a_module).

-include("a_header.hrl").
-include("a_folder/a_header.hrl").

-export([a_function/0]).
a_function() ->
  [application:get_env(my_config_from_app_src),
  application:get_env(environment)].