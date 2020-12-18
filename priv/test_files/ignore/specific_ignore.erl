-module(specific_ignore).

-hank([unused_macros]).

-define(UNUSED_MACRO, unused_macro).

-export([no_ignore/0]).

no_ignore(_, _) ->
    no_ignore.
