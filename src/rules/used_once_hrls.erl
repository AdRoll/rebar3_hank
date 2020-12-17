%% @doc A rule to detect header files used in just one module.
-module(used_once_hrls).

-behaviour(hank_rule).

-export([analyze/2]).

-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(_FilesAndASTs, _Context) ->
    [].
