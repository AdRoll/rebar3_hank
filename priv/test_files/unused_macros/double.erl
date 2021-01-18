-module(double).

-ifdef(X).
-define(Y, y).
-else.
-define(Y, z).
-endif.
