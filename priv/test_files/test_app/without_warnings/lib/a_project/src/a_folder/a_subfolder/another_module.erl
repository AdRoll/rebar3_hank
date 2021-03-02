-module(another_module).

-include("a_header.hrl").
-include("a_folder/a_header.hrl").

-export([a_function/0]).
a_function() ->
  [proplists:get_value(a_key, application:get_all_env(my_app)),
  application:get_env(other_config_from_app_src)].
