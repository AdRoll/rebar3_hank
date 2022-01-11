%% This file contains all the macros that are used or unused in the following modules:
%% - unused_macro_sample.erl
%% - used_macro_sample.erl
%%
%% Even when those files don't include this header, Hank will only report the
%% strictly unused macros as unused. It will act conservatively and not make any
%% effort to actually verify where each macro that's used is defined.
%% That's too hard. And if you have a project with multiple definitions of the
%% same macro with the same arity... well... as long as one of them is used,
%% none of them will be reported. Sorry.
-define(SIMPLE_MACRO, simple_macro).
-define(MACRO_USED_IN(MACRO), ??MACRO).
-define(MACRO_THAT_USES(OTHER_MACRO), {OTHER_MACRO, ?MACRO_USED_IN(?OTHER_MACRO)}).
-define(USED_MACROS_AS_FUNC, ?MODULE:simple_macros_func).
-define(UNUSED_MACRO, unused_macro).
-define(UNUSED_MACRO_WITH(), no_arguments).
-define(UNUSED_MACRO_WITH(ARGUMENTS), unused_macro_with(ARGUMENTS)).
-define(UNUSED_MACRO_WITH(TWO, ARGUMENTS), unused_macro_with(TWO, ARGUMENTS)).

-define( UNUSED_MACRO_WITH_BROKEN( CODE ) , case ?CODE of code -> ??CODE ) .

-define(UNUSED_MACRO_UNICODE_ÇØÍ, unused_macro_unicode_ÇØÍ).
%% This one is actually used, since it's one of used_macro_sample macros, too.
-define(macroIsAnAtom, macro_is_an_atom).
