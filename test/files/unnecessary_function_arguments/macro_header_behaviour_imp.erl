-module(macro_header_behaviour_imp).
-moduledoc """
This module includes a header file whose path is a macro, so Hank can't tell
which header file it is and assumes that it may be any of them.
""".

-define(HEADER, "behaviour_in_header.hrl").

-include(?HEADER).

-export([a_kind_of_magic/1]).

a_kind_of_magic(_) ->
    % this function won't be warned since it may be a callback
    implemented.
