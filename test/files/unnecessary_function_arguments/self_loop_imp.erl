-module(self_loop_imp).
-moduledoc """
This module includes a header file that includes itself and, through
another one, a header file declaring a local behaviour.
""".

-include("self_loop.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
