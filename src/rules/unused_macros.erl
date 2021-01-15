%% @doc A rule to detect unused macros.
%%      <p>To avoid this warning, remove the unused macros.</p>
%% @todo Extend the rule to check hrl files [https://github.com/AdRoll/rebar3_hank/issues/36]
%% @todo Detect unparsable macros [https://github.com/AdRoll/rebar3_hank/issues/37]
-module(unused_macros).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result
     || {File, AST} <- FilesAndASTs,
        filename:extension(File) == ".erl",
        Result <- do_analyze(File, AST)].

do_analyze(File, AST) ->
    FoldFun =
        fun(Node, {Definitions, Usage}) ->
           case erl_syntax:type(Node) of
               attribute ->
                   case hank_utils:attr_name(Node) of
                       define -> {[Node | Definitions], Usage};
                       _ -> {Definitions, Usage}
                   end;
               macro -> {Definitions, [Node | Usage]};
               _ -> {Definitions, Usage}
           end
        end,
    {MacroDefinitions, MacroUsage} =
        erl_syntax_lib:fold(FoldFun, {[], []}, erl_syntax:form_list(AST)),
    DefinedMacros = lists:map(fun hank_utils:macro_definition_name/1, MacroDefinitions),
    UsedMacros = lists:map(fun macro_application_name/1, MacroUsage),
    [result(File, MacroName, MacroArity, MacroDefinitions)
     || {MacroName, MacroArity} <- DefinedMacros -- UsedMacros].

macro_application_name(Node) ->
    {hank_utils:macro_name(Node), hank_utils:macro_arity(Node)}.

result(File, MacroName, MacroArity, MacroDefinitions) ->
    [Line] =
        [erl_anno:location(
             erl_syntax:get_pos(Md))
         || Md <- MacroDefinitions,
            hank_utils:macro_definition_name(Md) == {MacroName, MacroArity}],
    Text =
        case MacroArity of
            none ->
                hank_utils:format_text("?~ts is unused", [MacroName]);
            MacroArity ->
                hank_utils:format_text("?~ts/~p is unused", [MacroName, MacroArity])
        end,
    #{file => File,
      line => Line,
      text => Text}.
