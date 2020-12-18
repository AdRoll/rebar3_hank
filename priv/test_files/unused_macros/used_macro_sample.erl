-module(used_macro_sample).

-compile(export_all).

-define(SIMPLE_MACRO, simple_macro).
-define(MACRO_USED_IN(MACRO), ??MACRO).
-define(MACRO_THAT_USES(OTHER_MACRO), {OTHER_MACRO, ?MACRO_USED_IN(?OTHER_MACRO)}).

-define( USED_MACRO_WITH_BROKEN( CODE ) , case ?CODE of code -> ??CODE ) .

-define(macroIsAnAtom, macro_is_an_atom).
-define(macroIsAnAtom(With), {macro_is_an_atom, With}).
-define(macroIsAnAtomToo(), macro_is_an_atom).

using_macro(NotAMacro) ->
    ?MACRO_THAT_USES(NotAMacro).

simple_macro() ->
    ?SIMPLE_MACRO.

broken( ) -> ?USED_MACRO_WITH_BROKEN( SIMPLE_MACRO ) ; _ -> other end .

macro_is_an_atom() ->
    {?macroIsAnAtom, ?macroIsAnAtom(?macroIsAnAtomToo())}.
