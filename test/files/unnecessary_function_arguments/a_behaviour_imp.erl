%% @doc This module implements a local behaviour:
%%      The unnecessary_function_arguments rule will be ignored for the whole file.
-module(a_behaviour_imp).

-behaviour(a_behaviour).

-export([the_magic/0, a_kind_of_magic/1, function_with_ignored_arg/2]).

the_magic() ->
    implemented.

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

%% this will warn
function_with_ignored_arg(_, Value) ->
    Value.
