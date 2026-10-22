-module(no_header_behaviour_imp).
-moduledoc """
This module includes a header file, but not the one declaring the behaviour.
""".

-include("outer_header_without_behaviour.hrl").

-export([a_kind_of_magic/1]).

a_kind_of_magic(_) ->
    % this function will be warned since it's not a callback
    implemented.
