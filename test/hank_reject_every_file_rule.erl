%%% @doc This module is used by test_app_SUITE
-module(hank_reject_every_file_rule).

-behaviour(hank_rule).

-export([analyze/2]).

%% @doc All files are wrong!!
analyze(ASTs, _) ->
    [#{file => File,
       line => 1,
       text => "Test"}
     || {File, _} <- ASTs].
