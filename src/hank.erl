%%% @doc The Erlang Dead Code Cleaner
-module(hank).

%% It's dynamically called through rpc:pmap/3
-ignore_xref([get_ast/1]).

-export([analyze/5]).
-export([get_ast/1]).

-type ms() :: non_neg_integer().
-type stats() ::
    #{ignored := non_neg_integer(),
      parsing := ms(),
      analyzing => ms(),
      total => ms()}.
-type parsing_style() :: parallel | sequential.

-export_type([stats/0, parsing_style/0]).

%% @doc Runs a list of rules over a list of files and returns all the
%%      dead code pieces it can find.
-spec analyze([file:filename()],
              [hank_rule:ignore_spec()],
              [hank_rule:t()],
              parsing_style(),
              hank_context:t()) ->
                 #{results => [hank_rule:result()], stats => stats()}.
analyze(Files, IgnoredSpecsFromState, Rules, ParsingStyle, Context) ->
    StartMs = erlang:monotonic_time(millisecond),
    {ParsingNanos, ASTs} = timer:tc(fun() -> get_asts(Files, ParsingStyle) end),
    FilesAndASTs = lists:zip(Files, ASTs),
    IgnoredRulesFromAST =
        [{File, IgnoredRule, IgnoredSpecs}
         || {File, AST} <- FilesAndASTs,
            not lists:member({File, all, []}, IgnoredSpecsFromState),
            {IgnoredRule, IgnoredSpecs} <- ignored_rules(AST, Rules)],
    IgnoredRulesFromConfig =
        [{File, Rule, Options}
         || {File, IgnoredRule, Options} <- IgnoredSpecsFromState,
            Rule <- Rules,
            IgnoredRule == all orelse IgnoredRule == Rule],
    IgnoredRules = IgnoredRulesFromAST ++ IgnoredRulesFromConfig,
    erlang:yield(),
    {AnalyzingNanos, AllResults} =
        timer:tc(fun() -> analyze(Rules, FilesAndASTs, Context) end),
    {Results, Ignored} =
        lists:partition(fun(#{file := File,
                              rule := Rule,
                              pattern := Pattern}) ->
                           IgnoredSpecs = ignored_specs(File, Rule, IgnoredRules),
                           not hank_rule:is_ignored(Rule, Pattern, IgnoredSpecs)
                        end,
                        AllResults),
    TotalMs = erlang:monotonic_time(millisecond) - StartMs,
    #{results => Results,
      stats =>
          #{ignored => length(Ignored),
            parsing => ParsingNanos div 1000,
            analyzing => AnalyzingNanos div 1000,
            total => TotalMs}}.

-spec get_asts([file:filename()], parsing_style()) -> [erl_syntax:forms()].
get_asts(Files, parallel) ->
    rpc:pmap({?MODULE, get_ast}, [], Files);
get_asts(Files, sequential) ->
    lists:map(fun get_ast/1, Files).

%% @hidden Only used through rpc:pmap/3
-spec get_ast(file:filename()) -> erl_syntax:forms().
get_ast(File) ->
    case ktn_dodger:parse_file(File, [no_fail, parse_macro_definitions]) of
        {ok, AST} ->
            AST;
        {error, OpenError} ->
            erlang:error({cant_parse, File, OpenError})
    end.

ignored_rules(AST, Rules) ->
    FoldFun =
        fun(Node, Acc) ->
           case erl_syntax:type(Node) of
               attribute ->
                   try erl_syntax_lib:analyze_attribute(Node) of
                       {hank, {hank, ignore}} ->
                           normalize_ignored_rules(Rules) ++ Acc;
                       {hank, {hank, RulesToIgnore}} ->
                           normalize_ignored_rules(RulesToIgnore) ++ Acc;
                       _ ->
                           Acc
                   catch
                       _:_ ->
                           Acc
                   end;
               _ ->
                   Acc
           end
        end,
    erl_syntax_lib:fold(FoldFun, [], erl_syntax:form_list(AST)).

normalize_ignored_rules(RulesToIgnore) ->
    lists:map(fun normalize_ignored_rule/1, RulesToIgnore).

normalize_ignored_rule(Rule) when is_atom(Rule) ->
    {Rule, all};
normalize_ignored_rule({Rule, Specs}) when is_list(Specs) ->
    {Rule, Specs};
normalize_ignored_rule({Rule, Spec}) ->
    {Rule, [Spec]}.

ignored_specs(File, Rule, IgnoredRules) ->
    Fun = fun ({File0, Rule0, Specs}, IgnoredSpecs)
                  when File =:= File0 andalso Rule =:= Rule0 ->
                  case is_list(Specs) of
                      true ->
                          Specs ++ IgnoredSpecs;
                      false ->
                          [Specs | IgnoredSpecs]
                  end;
              ({File0, Rule0, all}, IgnoredSpecs) when File =:= File0 andalso Rule =:= Rule0 ->
                  [all | IgnoredSpecs];
              (_, IgnoredSpecs) ->
                  IgnoredSpecs
          end,
    lists:foldl(Fun, [], IgnoredRules).

analyze(Rules, ASTs, Context) ->
    [Result || Rule <- Rules, Result <- hank_rule:analyze(Rule, ASTs, Context)].
