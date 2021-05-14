%% @doc This module implements a_behaviour, if a
-module(unknown_behaviour_imp).

-behaviour(a_behaviour).
% This will be make ignoring the whole file in the tests
% because the "not_included_behaviour.erl" file is not sent to analize
-behaviour(not_included_behaviour).

-export([the_magic/0, a_kind_of_magic/1, function_with_ignored_arg/2]).

the_magic() ->
    implemented.

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.

%% this would warn but since the file is ignored it doesn't
function_with_ignored_arg(_, Value) ->
    Value.
