%%% @doc Behavior for defining a rule for Hank.
-module(hank_rule).

-type t() :: module().
-type asts() :: [{file:filename_all(), erl_syntax:forms()}].
-type result() ::
    #{file := file:filename_all(),
      line := non_neg_integer(),
      text := iodata()}.

                                 % 0 means the whole file

-export_type([t/0, result/0]).

-callback analyze(asts()) -> [result()].

-export([default_rules/0]).
-export([analyze/2]).

%% @doc The list of default rules to apply
%% @todo Retrieve the list of modules implementing this behavior
-spec default_rules() -> [].
default_rules() ->
    [].

%% @doc Analyze the given files with the rule.
-spec analyze(t(), asts()) -> [result()].
analyze(Rule, ASTs) ->
    Rule:analyze(ASTs).
