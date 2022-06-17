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

-export_type([t/0, asts/0, result/0, ignore_pattern/0, ignore_spec/0]).

-callback analyze(asts(), hank_context:t()) -> [result()].
-callback ignored(ignore_pattern(), term()) -> boolean().

-export([default_rules/0]).
-export([analyze/3]).
-export([is_ignored/3]).
-export([result_to_json/1]).

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
-spec is_ignored(t(), ignore_pattern(), all | term()) -> boolean().
is_ignored(Rule, Pattern, IgnoreSpec) ->
    IgnoreSpec =:= all orelse Rule:ignored(Pattern, IgnoreSpec).

-spec result_to_json(hank_rule:result()) -> map().
result_to_json(Data) ->
    #{file := FileName, line := Line, rule := RuleBroken, text := Description} = Data,
    #{<<"path">> => iolist_to_binary(FileName),
      <<"start_line">> => Line,
      <<"hank_rule_broken">> => atom_to_binary(RuleBroken),
      <<"title">> => compute_title(RuleBroken),
      <<"message">> => iolist_to_binary(Description)}.

-spec compute_title(atom()) -> binary().
compute_title(RuleBroken) ->
    case RuleBroken of
        unused_macros ->
            <<"Unused Macros">>;
        single_use_hrl_attrs ->
            <<"Macro only used once">>;
        unused_record_fields ->
            <<"Unused field in record">>;
        unused_hrls ->
            <<"Unused hrl files">>;
        unused_configuration_options ->
            <<"Unused config">>;
        unused_callbacks ->
            <<"Unused callback functions">>;
        unnecessary_function_arguments ->
            <<"Unused function arguments">>;
        single_use_hrls ->
            <<"Hrl file only used once">>
    end.
