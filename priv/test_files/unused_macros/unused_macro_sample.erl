-module(unused_macro_sample).

-compile(export_all).

-define(UNUSED_MACRO, unused_macro).
-define(UNUSED_MACRO_WITH(), no_arguments).
-define(UNUSED_MACRO_WITH(ARGUMENTS), unused_macro_with(ARGUMENTS)).
-define(UNUSED_MACRO_WITH(TWO, ARGUMENTS), unused_macro_with(TWO, ARGUMENTS)).

%% hank won't detect this one, since it's unparseable [https://github.com/AdRoll/rebar3_hank/issues/37]
-define( UNUSED_MACRO_WITH_BROKEN( CODE ) , case ?CODE of code -> ??CODE ) .
