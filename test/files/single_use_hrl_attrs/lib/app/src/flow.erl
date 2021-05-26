-module(flow).

-include("flow.hrl").

-export([ifndef/0, iif/0]).

-ifndef(IFNDEF).
ifndef() -> notdefined.
-else.
ifndef() -> defined.
-endif.


-ifdef(IFDEF).
-type ifdef() :: ifdef.
-export_type([ifdef/0]).
-endif.

-if(?IF(?VALUE)).

iif() -> true.

-elif(not ?IF(?VALUE)).

iif() -> false.

-else.

iif() -> wat.

-endif.

-undef(UNDEF).
