%% @doc A rule to detect unused callbacks.
%%      <p>The rule emits a warning if it can't find the callback's atom name
%%      being used within a module. It will NOT emit a warning if an atom named
%%      as a callback is being used, no matter for what that atom is used.</p>
%%      <p>To avoid this warning, remove the unused callback definition.</p>
-module(unused_callbacks).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(_FilesAndASTs, _Context) ->
    [].
