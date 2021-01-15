%% @doc A rule to detect hrl attributes used in just one module:
%%      Attributes supported:
%%      -define
%%      -record
%%      It will suggest to place those attributes inside the module to avoid
%%      having (and including) a hrl file.
-module(single_use_hrl_attrs).

-behaviour(hank_rule).

-export([analyze/2]).

%% @doc This builds a list of header files with its attributes.
%%      Then traverse the file ASTs mapping their macros and records
%%      And checks whether they were used just once.
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    HrlDefs = hrl_attrs(FilesAndASTs),
    AttributesUsed = lists:foldl(fun file_using/2, #{}, FilesAndASTs),
    [build_macro_result(HrlFile, MacroKey, AttributesUsed)
     || {HrlFile, #{define := Defines}} <- HrlDefs,
        MacroKey <- Defines,
        is_used_only_once(MacroKey, AttributesUsed)]
    ++ [build_record_result(HrlFile, RecordKey, AttributesUsed)
        || {HrlFile, #{record := Records}} <- HrlDefs,
           RecordKey <- Records,
           is_used_only_once(RecordKey, AttributesUsed)].

build_macro_result(HrlFile, {Macro, Line}, AttributesUsed) ->
    [File] = maps:get(Macro, AttributesUsed),
    Text =
        case Macro of
            {MacroName, none} ->
                iolist_to_binary(io_lib:format("?~s is used only at ~s", [MacroName, File]));
            {MacroName, MacroArity} ->
                iolist_to_binary(io_lib:format("?~s/~p is used only at ~s",
                                               [MacroName, MacroArity, File]))
        end,
    #{file => HrlFile,
      line => Line,
      text => Text}.

build_record_result(HrlFile, {Record, Line}, AttributesUsed) ->
    [File] = maps:get(Record, AttributesUsed),
    #{file => HrlFile,
      line => Line,
      text => iolist_to_binary(io_lib:format("#~p is used only at ~s", [Record, File]))}.

is_used_only_once({Key, _Line}, AttributesUsed) ->
    length(maps:get(Key, AttributesUsed, [])) == 1.

file_using({File, FileAST}, CurrentFiles) ->
    AddFun = fun(Files) -> lists:usort([File | Files]) end,
    FoldFun =
        fun(Node, Result) ->
           case erl_syntax:type(Node) of
               macro ->
                   Key = macro_application_name(Node),
                   maps:update_with(Key, AddFun, [File], Result);
               Attr
                   when Attr =:= record_expr;
                        Attr =:= record_access;
                        Attr =:= record_index_expr;
                        Attr =:= record_type ->
                   Key = record_name(Node, Attr),
                   maps:update_with(Key, AddFun, [File], Result);
               _ -> Result
           end
        end,
    erl_syntax_lib:fold(FoldFun, CurrentFiles, erl_syntax:form_list(FileAST)).

%% @doc It collects the hrl attrs like {file, [attrs]}
hrl_attrs(FilesAndASTs) ->
    [{File, attrs(AST)} || {File, AST} <- FilesAndASTs, filename:extension(File) == ".hrl"].

%% @doc A map with #{define => [], record => []} for each hrl tree
attrs(AST) ->
    FoldFun =
        fun(Node, #{define := Defines, record := Records} = Acc) ->
           case erl_syntax:type(Node) of
               attribute ->
                   case hank_utils:attr_name(Node) of
                       define ->
                           maps:put(define,
                                    [{hank_utils:macro_definition_name(Node), line(Node)}
                                     | Defines],
                                    Acc);
                       record ->
                           maps:put(record,
                                    [{record_definition_name(Node), line(Node)} | Records],
                                    Acc);
                       _ -> Acc
                   end;
               _ -> Acc
           end
        end,
    erl_syntax_lib:fold(FoldFun, #{define => [], record => []}, erl_syntax:form_list(AST)).

macro_application_name(Node) ->
    {hank_utils:macro_name(Node), hank_utils:macro_arity(Node)}.

record_definition_name(Node) ->
    try erl_syntax_lib:analyze_record_attribute(Node) of
        {RecordName, _} ->
            RecordName
    catch
        _:syntax_error ->
            %% There is a macro in the record definition
            ""
    end.

record_name(Node, Type) ->
    RecordName =
        case Type of
            record_expr ->
                erl_syntax:record_expr_type(Node);
            record_index_expr ->
                erl_syntax:record_index_expr_type(Node);
            record_access ->
                erl_syntax:record_access_type(Node);
            record_type ->
                erl_syntax:record_type_name(Node)
        end,
    erl_syntax:atom_value(RecordName).

line(Node) ->
    erl_anno:location(
        erl_syntax:get_pos(Node)).
