-module(export_all1).

-behaviour(ct_suite).

-compile(export_all).
-compile({inline, [a_test2/1]}).

all() -> [a_test].

a_test(_) -> a_test2().

a_test2() -> ok.
