%% @doc This module is a nasty case, it implements a behaviour inside a macro
-module(macro_behaviour_imp).

-define(WHY_DO_YOU_DO_THIS_TO_ME, a_behaviour).

-behaviour(?WHY_DO_YOU_DO_THIS_TO_ME).

-export([the_magic/0, a_kind_of_magic/1]).

the_magic() ->
    implemented.

a_kind_of_magic(_) ->
    % this function won't be warned since it's a callback
    implemented.
