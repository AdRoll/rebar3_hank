%%% @doc This module is used by test_app_SUITE
-module(hank_reject_every_file_rule).

-behaviour(hank_rule).

-export([analyze/1]).

%% @doc All files are wrong!!
analyze(ASTs) ->
    [#{file => File,
       line => 1,
       message => "Test"}
     || {File, _} <- ASTs].
