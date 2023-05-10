-module(export_all2).

-behaviour(ct_suite).

-compile([export_all]).

all() -> [a_test].

a_test(_) -> ok.
