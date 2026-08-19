-module(a_behaviour_imp).
-moduledoc """
This module implements a local behaviour:
The `unnecessary_function_arguments` rule will be ignored for all
exported functions since we can't tell which ones are dynamic callbacks.
""".

-behaviour(a_behaviour).

-export([the_magic/0, a_kind_of_magic/1, function_with_ignored_arg/2]).

the_magic() ->
    implemented.

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

-doc """
This exported function won't warn because the module implements a local behaviour.
""".
function_with_ignored_arg(_, Value) ->
    non_exported_function_with(ignored_arg, Value).

-doc """
This should produce a warning since, being a non-exported function, it can't
be a callback implementation.
""".
non_exported_function_with(_IgnoredArg, Value) ->
    Value.
