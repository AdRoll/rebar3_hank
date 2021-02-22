%% @doc A rule to detect unused callbacks.
%%      <p>This rule will check all callbacks defined in a module and find
%%      those that are not used anywhere in the module itself.</p>
%%      <p>It will emit a warning if it can't find the callback's atom name
%%      being used anywhere within a module. It will NOT emit a warning if an atom
%%      named as a callback is being used, no matter for what that atom is used.</p>
%%      <p>The assumption is that if you define a callback for a behavior,
%%      your generic module (where the callback is defined) should call
%%      that function at some point, using the implementation provided
%%      by the specific module (the one that implements the behavior).</p>
%%      <p>To avoid this warning, remove the unused callback definition.</p>
-module(unused_callbacks).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(_FilesAndASTs, _Context) ->
    %% @TODO WIP!
    [].
