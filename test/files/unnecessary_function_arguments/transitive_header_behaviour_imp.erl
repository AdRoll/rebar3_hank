-module(transitive_header_behaviour_imp).
-moduledoc """
This module implements a local behaviour declared in a header file that is
included by another header file.
""".

-include("outer_header.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
