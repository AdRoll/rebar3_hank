%% @doc A rule to detect unused macros.
%%      <p>To avoid this warning, remove the unused macros.</p>
%% @todo Extend the rule to check hrl files [https://github.com/AdRoll/rebar3_hank/issues/36]
%% @todo Detect unparsable macros [https://github.com/AdRoll/rebar3_hank/issues/37]
-module(unused_macros).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    MacrosInFiles = lists:map(fun macro_usage/1, FilesAndASTs),
    AllUsedMacros = [UsedMacro || #{used := Used} <- MacrosInFiles, UsedMacro <- Used],
    [Result
     || #{file := File,
          defined := DefinedMacros,
          used := UsedMacros}
            <- MacrosInFiles,
        Result <- analyze(File, DefinedMacros, UsedMacros, AllUsedMacros)].

macro_usage({File, AST}) ->
    FoldFun =
        fun(Node, {Definitions, Usage}) ->
           case erl_syntax:type(Node) of
               attribute ->
                   case hank_utils:attr_name(Node) of
                       define ->
                           {[Node | Definitions], Usage};
                       _ ->
                           {Definitions, Usage}
                   end;
               macro ->
                   {Definitions, [Node | Usage]};
               _ ->
                   {Definitions, Usage}
           end
        end,
    {MacroDefinitions, MacroUsage} =
        erl_syntax_lib:fold(FoldFun, {[], []}, erl_syntax:form_list(AST)),
    DefinedMacros = lists:map(fun macro_definition_name_and_line/1, MacroDefinitions),
    UsedMacros = lists:map(fun macro_application_name/1, MacroUsage),
    #{file => File,
      defined => DefinedMacros,
      used => UsedMacros}.

analyze(File, DefinedMacros, UsedMacros, AllUsedMacros) ->
    case filename:extension(File) of
        ".erl" ->
            analyze(File, DefinedMacros, UsedMacros);
        ".hrl" ->
            analyze(File, DefinedMacros, AllUsedMacros);
        _ ->
            []
    end.

analyze(File, DefinedMacros, UsedMacros) ->
    [result(File, MacroName, MacroArity, MacroLine)
     || {MacroName, MacroArity, MacroLine} <- DefinedMacros,
        not is_member({MacroName, MacroArity}, UsedMacros)].

macro_definition_name_and_line(Node) ->
    {MacroName, MacroArity} = hank_utils:macro_definition_name(Node),
    Line = hank_utils:node_line(Node),
    {MacroName, MacroArity, Line}.

macro_application_name(Node) ->
    {hank_utils:macro_name(Node), hank_utils:macro_arity(Node)}.

result(File, Name, Arity, Line) ->
    Text =
        case Arity of
            none ->
                hank_utils:format_text("?~ts is unused", [Name]);
            Arity ->
                hank_utils:format_text("?~ts/~p is unused", [Name, Arity])
        end,
    #{file => File,
      line => Line,
      text => Text,
      pattern => {Name, Arity}}.

%% @doc Rule ignore specifications. Example:
%%      <pre>
%%      -hank([{unused_macros,
%%              ["ALL", %% Will ignore ?ALL, ?ALL() and ?ALL(X)
%%               {"ZERO", 0}, %% Will ignore ?ZERO() but not ?ZERO(X) nor ?ZERO
%%               {"ONE",  1}, %% Will ignore ?ONE(X) but not ?ONE()   nor ?ONE
%%               {"NONE", none} %% Will ignore ?NONE but not ?NONE(X) nor ?NONE()
%%              ]},
%%      </pre>
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored({Name, Arity}, {Name, Arity}) ->
    true;
ignored({Name, _Arity}, Name) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.

is_member({MacroName, none}, UsedMacros) ->
    lists:keymember(MacroName, 1, UsedMacros);
is_member(Macro, UsedMacros) ->
    lists:member(Macro, UsedMacros).
