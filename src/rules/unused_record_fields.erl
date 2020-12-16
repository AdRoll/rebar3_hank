%% @doc A rule to detect unused record fields.
-module(unused_record_fields).

-behaviour(hank_rule).

-export([analyze/2]).

%% @todo Extend the rule to check hrl files [https://github.com/AdRoll/rebar3_hank/issues/33]
%% @todo Don't count record construction as usage [https://github.com/AdRoll/rebar3_hank/issues/35]
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result
     || {File, AST} <- FilesAndASTs,
        filename:extension(File) == ".erl",
        Result <- do_analyze(File, AST)].

do_analyze(File, AST) ->
    FoldFun =
        fun(Node, {Records, Usage}) ->
           case erl_syntax:type(Node) of
               attribute ->
                   case hank_utils:attribute_name(Node) of
                       record -> {[Node | Records], Usage};
                       _ -> {Records, Usage}
                   end;
               record_expr -> {Records, [Node | Usage]};
               record_access -> {Records, [Node | Usage]};
               record_index_expr -> {Records, [Node | Usage]};
               _ ->
                   % Ignored: record_field, typed_record_field, record_type, record_type_field
                   {Records, Usage}
           end
        end,
    {RecordDefinitions, RecordUsage} =
        erl_syntax_lib:fold(FoldFun, {[], []}, erl_syntax:form_list(AST)),
    DefinedFields =
        [{RecordName, FieldName}
         || Node <- RecordDefinitions,
            {RecordName, Fields} <- [erl_syntax_lib:analyze_record_attribute(Node)],
            {FieldName, _} <- Fields],
    UsedFields = lists:flatmap(fun analyze_record_expr/1, RecordUsage),
    [result(File, RecordName, FieldName, RecordDefinitions)
     || {RecordName, FieldName} <- DefinedFields -- UsedFields].

analyze_record_expr(Node) ->
    case erl_syntax_lib:analyze_record_expr(Node) of
        {record_expr, {RecordName, Fields}} ->
            [{RecordName, FieldName} || {FieldName, _} <- Fields];
        {_, {RecordName, FieldName}} ->
            [{RecordName, FieldName}]
    end.

result(File, RecordName, FieldName, RecordDefinitions) ->
    L = case find_record_definition(RecordName, RecordDefinitions) of
            false ->
                0;
            {value, RecordDefinition} ->
                [_, RecordFields] = erl_syntax:attribute_arguments(RecordDefinition),
                case find_record_field(FieldName, erl_syntax:tuple_elements(RecordFields)) of
                    false ->
                        erl_anno:location(
                            erl_syntax:get_pos(RecordDefinition));
                    {value, FieldDefinition} ->
                        erl_anno:location(
                            erl_syntax:get_pos(FieldDefinition))
                end
        end,
    #{file => File,
      line => L,
      text =>
          iolist_to_binary(io_lib:format("Field ~p in record ~p is unused",
                                         [FieldName, RecordName]))}.

find_record_definition(RecordName, Definitions) ->
    lists:search(fun(Definition) ->
                    case erl_syntax:attribute_arguments(Definition) of
                        [RN | _] -> erl_syntax:concrete(RN) == RecordName;
                        [] -> false
                    end
                 end,
                 Definitions).

find_record_field(FieldName, Definitions) ->
    lists:search(fun(Definition) ->
                    {FN, _} = erl_syntax_lib:analyze_record_field(Definition),
                    FN == FieldName
                 end,
                 Definitions).
