-module(ring2_b_imp).
-moduledoc """
This module includes the header file of an include cycle between two header files that
doesn't declare the local behaviour, but includes the one that does.
""".

-include("ring2_b.hrl").

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    non_exported_function_with(ignored_arg, implemented).

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

non_exported_function_with(_IgnoredArg, Value) ->
    Value.
