-module(flow).

-export([iif/0]).

-ifndef(IFNDEF).
-define(IFNDEF, true).
-endif.

-define(IFDEF, true).
-ifdef(IFDEF).
-type ifdef() :: ifdef.
-export_type([ifdef/0]).
-endif.

-define(IF(X), not X).

-if(?IF(?VALUE)).

iif() -> true.

-elif(not ?IF(?VALUE)).

iif() -> false.

-else.

iif() -> wat.

-endif.

-define(UNDEF, undef).
-undef(UNDEF).
