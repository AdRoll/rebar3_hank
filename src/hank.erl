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
                 #{results := [hank_rule:result()],
                   unused_ignores := [hank_rule:ignore_spec()],
                   stats := stats()}.
analyze(Files, IgnoreSpecsFromState, Rules, ParsingStyle, Context) ->
    StartMs = erlang:monotonic_time(millisecond),
    {ParsingNanos, ASTs} = timer:tc(fun() -> get_asts(Files, ParsingStyle) end),
    FilesAndASTs = lists:zip(Files, ASTs),
    HankAttributes =
        [{File, Attribute} || {File, AST} <- FilesAndASTs, Attribute <- hank_attributes(AST)],
    IgnoreRulesFromAST =
        [{File, IgnoreRule, IgnoreSpecs}
         || {File, Attribute} <- HankAttributes,
            not lists:member({File, all, []}, IgnoreSpecsFromState),
            {IgnoreRule, IgnoreSpecs} <- normalize_ignored_rules(Attribute, Rules)],
    IgnoreRulesFromConfig =
        [{File, Rule, Options}
         || {File, IgnoreRule, Options} <- IgnoreSpecsFromState,
            Rule <- Rules,
            IgnoreRule == all orelse IgnoreRule == Rule],
    WhollyIgnoredFiles =
        lists:usort([File || {File, all, _Options} <- IgnoreSpecsFromState]
                    ++ [File || {File, ignore} <- HankAttributes]),
    IgnoreRules = IgnoreRulesFromAST ++ IgnoreRulesFromConfig,
    erlang:yield(),
    {AnalyzingNanos, AllResults} =
        timer:tc(fun() -> analyze(Rules, FilesAndASTs, Context) end),
    {Results, Ignored} = remove_ignored_results(AllResults, IgnoreRules),
    UnusedIgnores = unused_ignore_specs(WhollyIgnoredFiles, IgnoreRules, Ignored),
    TotalMs = erlang:monotonic_time(millisecond) - StartMs,
    #{results => Results,
      unused_ignores => UnusedIgnores,
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

hank_attributes(AST) ->
    FoldFun =
        fun(Node, Acc) ->
           case erl_syntax:type(Node) of
               attribute ->
                   try erl_syntax_lib:analyze_attribute(Node) of
                       {hank, {hank, Something}} ->
                           [Something | Acc];
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

normalize_ignored_rules(ignore, Rules) ->
    lists:map(fun normalize_ignored_rule/1, Rules);
normalize_ignored_rules(RulesToIgnore, _) ->
    lists:map(fun normalize_ignored_rule/1, RulesToIgnore).

normalize_ignored_rule(Rule) when is_atom(Rule) ->
    {Rule, all};
normalize_ignored_rule({Rule, Specs}) ->
    {Rule, Specs}.

remove_ignored_results(AllResults, IgnoreRules) ->
    remove_ignored_results(AllResults, IgnoreRules, {[], []}).

remove_ignored_results([], _, {FilteredResults, IgnoredResults}) ->
    {lists:reverse(FilteredResults), lists:reverse(IgnoredResults)};
remove_ignored_results([Result | Results],
                       IgnoreRules,
                       {FilteredResults, IgnoredResults}) ->
    #{file := File,
      rule := Rule,
      pattern := Pattern} =
        Result,
    IgnoreSpecs = ignore_specs(File, Rule, IgnoreRules),
    NewAcc =
        case lists:search(fun(IgnoreSpec) -> hank_rule:is_ignored(Rule, Pattern, IgnoreSpec) end,
                          IgnoreSpecs)
        of
            {value, IgnoreSpec} ->
                {FilteredResults, [Result#{ignore_spec => IgnoreSpec} | IgnoredResults]};
            false ->
                {[Result | FilteredResults], IgnoredResults}
        end,
    remove_ignored_results(Results, IgnoreRules, NewAcc).

ignore_specs(File, Rule, IgnoreRules) ->
    Fun = fun ({File0, Rule0, Specs}, IgnoreSpecs)
                  when File =:= File0 andalso Rule =:= Rule0 ->
                  case is_list(Specs) of
                      true ->
                          Specs ++ IgnoreSpecs;
                      false ->
                          [Specs | IgnoreSpecs]
                  end;
              (_, IgnoreSpecs) ->
                  IgnoreSpecs
          end,
    lists:foldl(Fun, [], IgnoreRules).

unused_ignore_specs(WhollyIgnoredFiles, IgnoreRules, IgnoredResults) ->
    FilteredIgnoreRules =
        [IR || IR = {File, _, _} <- IgnoreRules, not lists:member(File, WhollyIgnoredFiles)],
    lists:foldl(fun ({File, Rule, all}, Acc) ->
                        case unused_ignored_spec(File, Rule, all, IgnoredResults) of
                            true ->
                                [{File, Rule, all} | Acc];
                            false ->
                                Acc
                        end;
                    ({File, Rule, Specs}, Acc) ->
                        case [Spec
                              || Spec <- Specs,
                                 unused_ignored_spec(File, Rule, Spec, IgnoredResults)]
                        of
                            [] ->
                                Acc;
                            UnusedSpecs ->
                                [{File, Rule, UnusedSpecs} | Acc]
                        end
                end,
                [],
                FilteredIgnoreRules).

unused_ignored_spec(File, Rule, Spec, IgnoredResults) ->
    not
        lists:any(fun(#{file := File0,
                        rule := Rule0,
                        ignore_spec := Spec0}) ->
                     File == File0 andalso Rule == Rule0 andalso Spec == Spec0
                  end,
                  IgnoredResults).

analyze(Rules, ASTs, Context) ->
    [Result || Rule <- Rules, Result <- hank_rule:analyze(Rule, ASTs, Context)].
