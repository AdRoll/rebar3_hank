%%% @doc Behavior for defining a rule for Hank.
-module(hank_rule).

-type t() :: module().
-type asts() :: [{file:filename_all(), erl_syntax:forms()}].
-type result() ::
    #{file := file:filename_all(),
      line := non_neg_integer(),
      text := iodata()}.

-export_type([t/0, result/0]).

-callback analyze(asts(), hank_context:t()) -> [result()].

-export([default_rules/0]).
-export([analyze/3]).

%% @doc The list of default rules to apply
%% @todo Retrieve the list of modules implementing this behavior
-spec default_rules() -> [].
default_rules() ->
    [].

%% @doc Analyze the given files with the rule.
-spec analyze(t(), asts(), hank_context:t()) -> [result()].
analyze(Rule, ASTs, Context) ->
    Rule:analyze(ASTs, Context).
