%% @doc A rule to detect hrl attributes used in just one module:
%%      Attributes supported:
%%      -define
%%      -record
%%      It will suggest to place those attributes inside the module to avoid
%%      having (and including) a hrl file.
%%
%%      <h3>Note</h3>
%%      <blockquote>
%%      This rule assumes that hrl files will not be used outside your project.
%%      If you are writing a library that requires your clients to use some of
%%      your header files and attributes, you can add an ignore rule in
%%      rebar.config for it.
%%      </blockquote>
-module(single_use_hrl_attrs).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

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
        is_used_only_once(HrlFile, MacroKey, AttributesUsed)]
    ++ [build_record_result(HrlFile, RecordKey, AttributesUsed)
        || {HrlFile, #{record := Records}} <- HrlDefs,
           RecordKey <- Records,
           is_used_only_once(HrlFile, RecordKey, AttributesUsed)].

build_macro_result(HrlFile, {Macro, Line}, AttributesUsed) ->
    [File] = maps:get(Macro, AttributesUsed),
    Text =
        case Macro of
            {MacroName, none} ->
                hank_utils:format_text("?~ts is used only at ~ts", [MacroName, File]);
            {MacroName, MacroArity} ->
                hank_utils:format_text("?~ts/~tp is used only at ~ts",
                                       [MacroName, MacroArity, File])
        end,
    #{file => HrlFile,
      line => Line,
      text => Text,
      pattern => Macro}.

build_record_result(HrlFile, {Record, Line}, AttributesUsed) ->
    [File] = maps:get(Record, AttributesUsed),
    #{file => HrlFile,
      line => Line,
      text => hank_utils:format_text("#~tp is used only at ~ts", [Record, File]),
      pattern => Record}.

is_used_only_once(HrlFile, {Key, _Line}, AttributesUsed) ->
    case maps:get(Key, AttributesUsed, []) of
        [SingleFile] ->
            %% There is nothing wrong with using an attribute only in the same
            %% file where it's defined.
            SingleFile /= HrlFile;
        _ ->
            false
    end.

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
               _ ->
                   Result
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
                       _ ->
                           Acc
                   end;
               _ ->
                   Acc
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
    hank_utils:node_line(Node).

%% @doc Rule ignore specifications. Example:
%%      <pre>
%%      -hank([{single_use_hrl_attrs,
%%              ["ALL",          %% Will ignore ?ALL, ?ALL() and ?ALL(X)
%%               {"ZERO", 0},    %% Will ignore ?ZERO() but not ?ZERO(X) nor ?ZERO
%%               {"ONE",  1},    %% Will ignore ?ONE(X) but not ?ONE()   nor ?ONE
%%               {"NONE", none}, %% Will ignore ?NONE but not ?NONE(X) nor ?NONE()
%%               record_name     %% Will ignore #record_name
%%              ]},
%%      </pre>
-spec ignored(hank_rule:ignore_pattern(), term()) -> false.
ignored({MacroName, Arity}, {MacroName, Arity}) ->
    true;
ignored({MacroName, _Arity}, MacroName) ->
    true;
ignored(RecordName, RecordName) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.
