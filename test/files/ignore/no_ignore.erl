-module(no_ignore).

%% This breaks erl_syntax_lib:analyze_attribute
-ignore_xref([{?MODULE, no_ignore, 0}]).

-export([no_ignore/0]).

no_ignore(_, _) ->
    no_ignore.
