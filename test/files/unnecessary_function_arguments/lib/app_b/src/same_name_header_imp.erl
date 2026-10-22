-module(same_name_header_imp).
-moduledoc """
This module includes its own app's `shared.hrl`, which declares no behaviour.
Another app has a header file with the same name that does declare one and,
since Hank can't tell which one is included, it assumes that it may be either.
""".

-include("shared.hrl").

-export([a_kind_of_magic/1]).

a_kind_of_magic(_) ->
    % this function won't be warned since it may be a callback
    implemented.
