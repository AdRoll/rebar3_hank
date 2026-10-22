-module(ifdef_header_behaviour_imp).
-moduledoc """
This module includes a header file that declares a local behaviour only if a
macro is defined. Hank doesn't evaluate the condition, so the behaviour counts.
""".

-include("ifdef_behaviour_in_header.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it may be a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
