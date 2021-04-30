-module(another_module).

-include("a_header.hrl").
-include("a_folder/a_header.hrl").

-define(A_COMPLEX_MACRO,
        fun() ->
           All = application:get_all_env(test_app),
           proplists:get_value(this_used_key, All)
        end).
-define(A_COMPLEX_MACRO(App),
        fun() ->
           All = application:get_all_env(App),
           proplists:get_value(another_used_key, All)
        end).
-define(A_COMPLEX_MACRO(),
        fun() ->
           Value = application:get_env(yet_another_key),
           list_to_binary(Value)
        end).
-define(A_MACRO_FUN(), fun() -> application:get_env(yet_another_key_2) end).
-define(A_MACRO_FUN_USED, ?A_MACRO_FUN()).
-define(A_MACRO_WITH_BODY,
        begin
            erlang:time(),
            application:get_env(yet_another_key_3)
        end).

-record(state, {foo = application:get_env(my_great_option), bar = ok}).

-export([a_function/0]).

a_function() ->
    R = #state{},
    R#state{bar = application:get_env(another_great_option)},
    [proplists:get_value(a_key, application:get_all_env(my_app)),
     ?A_COMPLEX_MACRO,
     ?A_COMPLEX_MACRO(test_app),
     ?A_COMPLEX_MACRO(),
     ?A_MACRO_FUN_USED,
     R#state.foo,
     R#state.bar,
     ?A_MACRO_WITH_BODY,
     application:get_env(other_config_from_app_src)].
