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
                   case hank_utils:attribute_name(Node) of
                       define -> {[Node | Definitions], Usage};
                       _ -> {Definitions, Usage}
                   end;
               macro -> {Definitions, [Node | Usage]};
               _ -> {Definitions, Usage}
           end
        end,
    {MacroDefinitions, MacroUsage} =
        erl_syntax_lib:fold(FoldFun, {[], []}, erl_syntax:form_list(AST)),
    DefinedMacros = lists:map(fun macro_definition_name/1, MacroDefinitions),
    UsedMacros = lists:map(fun macro_application_name/1, MacroUsage),
    [result(File, MacroName, MacroArity, MacroDefinitions)
     || {MacroName, MacroArity} <- DefinedMacros -- UsedMacros].

macro_definition_name(Node) ->
    [MacroNameNode | _] = erl_syntax:attribute_arguments(Node),
    case erl_syntax:type(MacroNameNode) of
        application ->
            MacroName =
                erl_syntax:variable_literal(
                    erl_syntax:application_operator(MacroNameNode)),
            MacroArity = length(erl_syntax:application_arguments(MacroNameNode)),
            {MacroName, MacroArity};
        variable ->
            {erl_syntax:variable_literal(MacroNameNode), none}
    end.

macro_application_name(Node) ->
    MacroName =
        erl_syntax:variable_literal(
            erl_syntax:macro_name(Node)),
    MacroArity =
        case erl_syntax:macro_arguments(Node) of
            none ->
                none;
            Args ->
                length(Args)
        end,
    {MacroName, MacroArity}.

result(File, MacroName, MacroArity, MacroDefinitions) ->
    [Line] =
        [erl_anno:location(
             erl_syntax:get_pos(Md))
         || Md <- MacroDefinitions, macro_definition_name(Md) == {MacroName, MacroArity}],
    Text =
        case MacroArity of
            none ->
                iolist_to_binary(io_lib:format("?~s is unused", [MacroName]));
            MacroArity ->
                iolist_to_binary(io_lib:format("?~s/~p is unused", [MacroName, MacroArity]))
        end,
    #{file => File,
      line => Line,
      text => Text}.
