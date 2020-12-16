%% @doc A rule to detect unused record fields.
-module(unused_record_fields).

-behaviour(hank_rule).

-export([analyze/2]).

%% @todo Extend the rule to check hrl files [https://github.com/AdRoll/rebar3_hank/issues/33]
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result
     || {File, AST} <- FilesAndASTs,
        filename:extension(File) == ".erl",
        Result <- do_analyze(File, AST)].

do_analyze(File, AST) ->
    FoldFun =
        fun(Node, {RFs, RDs, RU}) ->
           case erl_syntax:type(Node) of
               attribute ->
                   case erl_syntax_lib:analyze_attribute(Node) of
                       {record, {Name, Fields}} ->
                           {[{Name, Field} || {Field, _} <- Fields] ++ RDs, RU};
                       _ -> {RDs, RU}
                   end;
               record_field -> {[Node | RFs], RDs, RU};
               record_access -> {RDs, [used_field(Node) | RU]};
               record_index_expr -> {RDs, [used_field(Node) | RU]};
               _ -> % Ignored: record_expr, record_field, typed_record_field, record_type, record_type_field
                   {RDs, RU}
           end
        end,
    {RecordFields, DefinedFields, UsedFields} =
        erl_syntax_lib:fold(FoldFun, {[], [], []}, erl_syntax:form_list(AST)),
    ct:pal("File = ~p\nRecordFields = ~p\nDefinedFields = ~p\nUsedFields "
           "= ~p",
           [File, RecordFields, DefinedFields, UsedFields]),
    [].

used_field(Node) ->
    {_, Field} = erl_syntax_lib:analyze_record_expr(Node),
    Field.
