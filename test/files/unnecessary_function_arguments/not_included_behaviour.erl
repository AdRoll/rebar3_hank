-module(not_included_behaviour).

-callback the_not_included() -> any().

-export([the_not_included/1]).

the_not_included(M) ->
    M:the_not_included().
