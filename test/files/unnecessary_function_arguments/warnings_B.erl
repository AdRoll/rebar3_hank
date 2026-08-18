-module(warnings_B).

-export([underscore/3, not_underscored/2]).

-doc """
`_` is unused.
""".
underscore(_, Arg2, Arg3) ->
    {Arg2, Arg3}.

-doc """
`Arg2` is unused and not underscored (although the linter should complain).
""".
not_underscored(Arg1, Arg2) ->
    Arg1.
