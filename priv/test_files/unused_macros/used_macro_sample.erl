-module(used_macro_sample).

-compile(export_all).

-define(SIMPLE_MACRO, simple_macro).
-define(MACRO_USED_IN(MACRO), ??MACRO).
-define(MACRO_THAT_USES(OTHER_MACRO), {OTHER_MACRO, ?MACRO_USED_IN(?OTHER_MACRO)}).

-define( USED_MACRO_WITH_BROKEN( CODE ) , case ?CODE of code -> ??CODE ) .

using_macro(NotAMacro) ->
    ?MACRO_THAT_USES(NotAMacro).

simple_macro() ->
    ?SIMPLE_MACRO.

broken( ) -> ?USED_MACRO_WITH_BROKEN( SIMPLE_MACRO ) ; _ -> other end .
