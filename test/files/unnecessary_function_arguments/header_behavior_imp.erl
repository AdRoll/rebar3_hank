-module(header_behavior_imp).
-moduledoc """
This module implements a local behaviour declared in an included header file
with the US spelling of the attribute.
""".

-include("behavior_in_header.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
