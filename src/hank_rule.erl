%%% @doc Behavior for defining a rule for Hank.
-module(hank_rule).

-type t() :: module().
-type asts() :: [{file:filename(), erl_syntax:forms()}].
-type result() ::
    #{file := file:filename(),
      line := non_neg_integer(),
      text := iodata(),
      rule => t(),
      pattern => ignore_pattern()}.
-type ignore_pattern() :: undefined | tuple().
-type ignore_spec() :: {file:filename(), t() | all} | {file:filename(), t(), term()}.

-export_type([t/0, result/0, ignore_pattern/0, ignore_spec/0]).

-callback analyze(asts(), hank_context:t()) -> [result()].
-callback ignored(ignore_pattern(), term()) -> boolean().

-export([default_rules/0]).
-export([analyze/3]).
-export([is_ignored/3]).

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
    try
        [Result#{rule => Rule} || Result <- Rule:analyze(ASTs, Context)]
    catch
        _:Error:Stack ->
            logger:error("~p:analyze/3 failed with Error ~p \nStack: ~p", [Rule, Error, Stack]),
            erlang:error(analize_error)
    end.

%% @doc Check if given rule should be ignored from results
-spec is_ignored(t(), ignore_pattern(), [all | term()]) -> boolean().
is_ignored(Rule, Pattern, IgnoredSpecs) ->
    lists:any(fun(IgnoreSpec) -> IgnoreSpec =:= all orelse Rule:ignored(Pattern, IgnoreSpec)
              end,
              IgnoredSpecs).
