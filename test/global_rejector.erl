%%% @doc This module is used by test_app_SUITE
-module(global_rejector).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% @doc All files are wrong!!
analyze(ASTs, _) ->
    [#{file => File,
       line => 1,
       text => "global_rejector",
       pattern => undefined}
     || {File, _} <- ASTs].

-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored(_Pattern, _IgnoreSpec) ->
    false.
