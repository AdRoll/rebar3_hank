-module(a_behaviour).

-callback the_magic() -> any().
-callback a_kind_of_magic(any()) -> any().

-export([the_magic/1, a_kind_of_magic/1, a_function_from_the_behaviour/1]).

the_magic(M) ->
    M:the_magic().

a_kind_of_magic(M) ->
    M:a_kind_of_magic().

%% this will warn
a_function_from_the_behaviour(_) ->
    argument_ignored.
