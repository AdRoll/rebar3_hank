%%% @doc Behavior for defining a rule for Hank.
-module(hank_rule).

-type t() :: module().
-type asts() :: [{file:filename(), erl_syntax:forms()}].
-type result() ::
    #{file := file:filename(),
      line := non_neg_integer(),
      text := iodata(),
      rule => t()}.

-export_type([t/0, result/0]).

-callback analyze(asts(), hank_context:t()) -> [result()].

-export([default_rules/0]).
-export([analyze/3]).

%% @doc The list of default rules to apply
-spec default_rules() -> [].
default_rules() ->
    [Module
     || File
            <- filelib:wildcard(
                   filename:join([code:lib_dir(rebar3_hank), "**/*.beam"])),
        Module <- [list_to_atom(filename:basename(File, ".beam"))],
        {behaviour, Behaviours} <- Module:module_info(attributes),
        lists:member(?MODULE, Behaviours)].

%% @doc Analyze the given files with the rule.
-spec analyze(t(), asts(), hank_context:t()) -> [result()].
analyze(Rule, ASTs, Context) ->
    [Result#{rule => Rule} || Result <- Rule:analyze(ASTs, Context)].
