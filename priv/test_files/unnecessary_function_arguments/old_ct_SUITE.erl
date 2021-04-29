-module(old_ct_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([a_test/1]).

all() ->
    [a_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    unused.

a_test(_Config) ->
    unused.
