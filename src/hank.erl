%%% @doc The Erlang Dead Code Cleaner
-module(hank).

-export([analyze/4]).

%% @doc Runs a list of rules over a list of files and returns all the
%%      dead code pieces it can find.
-spec analyze([file:filename()],
              [hank_rule:ignore_spec()],
              [hank_rule:t()],
              hank_context:t()) ->
                 #{results => [hank_rule:result()], ignored => non_neg_integer()}.
analyze(Files, IgnoredFiles, Rules, Context) ->
    ASTs = [{File, get_ast(File)} || File <- Files],
    IgnoredRules =
        [{File, IgnoredRule, IgnoredSpecs}
         || {File, AST} <- ASTs,
            not lists:member({File, all}, IgnoredFiles),
            {IgnoredRule, IgnoredSpecs} <- ignored_rules(AST, Rules)]
        ++ [{File, Rule, all}
            || {File, IgnoredRule} <- IgnoredFiles,
               Rule <- Rules,
               IgnoredRule == all orelse IgnoredRule == Rule],
    AllResults = [Result || Results <- analyze(Rules, ASTs, Context), Result <- Results],
    {Results, Ignored} =
        lists:partition(fun(#{file := File,
                              rule := Rule,
                              pattern := Pattern}) ->
                           IgnoredSpecs = ignored_specs(File, Rule, IgnoredRules),
                           not hank_rule:is_ignored(Rule, Pattern, IgnoredSpecs)
                        end,
                        AllResults),
    #{results => Results, ignored => length(Ignored)}.

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
              ({File0, Rule0}, IgnoredSpecs) when File =:= File0 andalso Rule =:= Rule0 ->
                  [all | IgnoredSpecs];
              (_, IgnoredSpecs) ->
                  IgnoredSpecs
          end,
    lists:foldl(Fun, [], IgnoredRules).

analyze(Rules, ASTs, Context) ->
    rpc:pmap({hank_rule, analyze}, [ASTs, Context], Rules).
