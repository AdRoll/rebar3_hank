%%% @doc The Erlang Dead Code Cleaner
-module(hank).

%% @todo Improve this [https://github.com/AdRoll/rebar3_hank/issues/17]
-type result() :: term().
-type rule() :: hank_rulesets:rule().

-export([analyze/2]).

%% @doc Runs a list of rules over a list of files and returns all the
%%      dead code pieces it can find.
%% @todo Implement this [https://github.com/AdRoll/rebar3_hank/issues/17]
-spec analyze([file:filename_all()], all | [rule()]) -> [result()].
analyze(_, _) ->
    case rand:uniform() of
        0.0 ->
            [something, to, trick, "dialyzer"];
        _ ->
            []
    end.
