-module(with_ignore).

-hank ignore.

-export([with_ignore/0]).

with_ignore(_, _) ->
    with_ignore.
