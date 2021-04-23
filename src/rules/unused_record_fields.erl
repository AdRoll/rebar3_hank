%% @doc A rule to detect unused record fields.
%%      <p>The rule will detect fields that are defined as part of a record but
%%      never actually used anywhere.</p>
%%      <p>To avoid this warning, remove the unused record fields.</p>
%% @todo Extend the rule to check hrl files [https://github.com/AdRoll/rebar3_hank/issues/33]
%% @todo Don't count record construction as usage [https://github.com/AdRoll/rebar3_hank/issues/35]
-module(unused_record_fields).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% @private
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
                   case hank_utils:attr_name(Node) of
                       record ->
                           {[Node | Records], Usage};
                       _ ->
                           {Records, Usage}
                   end;
               record_expr ->
                   {Records, [Node | Usage]};
               record_access ->
                   {Records, [Node | Usage]};
               record_index_expr ->
                   {Records, [Node | Usage]};
               _ ->
                   % Ignored: record_field, typed_record_field, record_type, record_type_field
                   {Records, Usage}
           end
        end,
    {RecordDefinitions, RecordUsage} =
        erl_syntax_lib:fold(FoldFun, {[], []}, erl_syntax:form_list(AST)),
    DefinedFields =
        [{RecordName, FieldName}
         || Node <- RecordDefinitions, {RecordName, FieldName} <- analyze_record_attribute(Node)],
    {UsedRecords, UsedFields} =
        lists:foldl(fun(Node, {URs, UFs}) ->
                       case analyze_record_expr(Node) of
                           {RecordName, all_fields} ->
                               {[RecordName | URs], UFs};
                           Fields ->
                               {URs, Fields ++ UFs}
                       end
                    end,
                    {[], []},
                    RecordUsage),
    [result(File, RecordName, FieldName, RecordDefinitions)
     || {RecordName, FieldName} <- DefinedFields -- UsedFields,
        not lists:member(RecordName, UsedRecords)].

analyze_record_attribute(Node) ->
    try erl_syntax_lib:analyze_record_attribute(Node) of
        {RecordName, Fields} ->
            [{RecordName, FieldName} || {FieldName, _} <- Fields]
    catch
        _:syntax_error ->
            %% There is a macro in the record definition
            []
    end.

analyze_record_expr(Node) ->
    try erl_syntax_lib:analyze_record_expr(Node) of
        {record_expr, {RecordName, Fields}} ->
            [{RecordName, FieldName} || {FieldName, _} <- Fields];
        {_, {RecordName, FieldName}} ->
            [{RecordName, FieldName}]
    catch
        _:syntax_error ->
            %% Probably the record expression uses stuff like #{_ = '_'} or Macros
            RecordName =
                case erl_syntax:type(Node) of
                    record_expr ->
                        erl_syntax:record_expr_type(Node);
                    record_index_expr ->
                        erl_syntax:record_index_expr_type(Node);
                    record_access ->
                        erl_syntax:record_access_type(Node)
                end,
            {erl_syntax:atom_value(RecordName), all_fields}
    end.

result(File, RecordName, FieldName, RecordDefinitions) ->
    L = case find_record_definition(RecordName, RecordDefinitions) of
            false ->
                0;
            {value, RecordDefinition} ->
                [_, RecordFields] = erl_syntax:attribute_arguments(RecordDefinition),
                case find_record_field(FieldName, erl_syntax:tuple_elements(RecordFields)) of
                    false ->
                        hank_utils:node_line(RecordDefinition);
                    {value, FieldDefinition} ->
                        hank_utils:node_line(FieldDefinition)
                end
        end,
    #{file => File,
      line => L,
      text =>
          hank_utils:format_text("Field ~tp in record ~tp is unused", [FieldName, RecordName]),
      pattern => {RecordName, FieldName}}.

find_record_definition(RecordName, Definitions) ->
    lists:search(fun(Definition) ->
                    case erl_syntax:attribute_arguments(Definition) of
                        [RN | _] ->
                            erl_syntax:type(RN) == atom
                            andalso erl_syntax:atom_value(RN) == RecordName;
                        [] ->
                            false
                    end
                 end,
                 Definitions).

find_record_field(FieldName, Definitions) ->
    lists:search(fun(Definition) ->
                    {FN, _} = erl_syntax_lib:analyze_record_field(Definition),
                    FN == FieldName
                 end,
                 Definitions).

%% @doc Ignore particular fields or all the fields in a record
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored({RecordName, FieldName}, {RecordName, FieldName}) ->
    true;
ignored({RecordName, _FieldName}, RecordName) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.
