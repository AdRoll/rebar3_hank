-module(export_all1).

-behaviour(ct_suite).

-compile(export_all).

all() -> [a_test].

a_test(_) -> ok.
