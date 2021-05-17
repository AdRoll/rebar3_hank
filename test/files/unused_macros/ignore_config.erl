-module(ignore_config).

%% @doc No warnings should be emitted for this file since
%%      rebar.config specifically states that all of them
%%      should be ignored.

-define(MACRO_ALL, "this macro is always ignored").
-define(MACRO_ALL(), "regardless of its arity").
-define(MACRO_ALL(It), "is never reported as unused").
-define(MACRO_ALL(Not, Even), "if it has multiple arguments").
-define(MACRO_0(), "This one should since it has 0 arguments").
-define(MACRO_1(But), "not for this 1-aritied version").
-define(MACRO_NONE, "This instance should be ignored").
