-module(ring3_c_imp).
-moduledoc """
This module includes the last header file of an include cycle between three header files
whose first header file declares a local behaviour.
""".

-include("ring3_c.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
