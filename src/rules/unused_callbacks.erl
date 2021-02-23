%% @doc A rule to detect unused callbacks.
%%      <p>This rule will check all callbacks defined in a module and find
%%      those that are not used anywhere in the module itself.</p>
%%      <p>It will emit a warning if it can't find the callback's atom name
%%      being used anywhere within a module. It will NOT emit a warning if an atom
%%      named as a callback is being used, no matter for what that atom is used.</p>
%%      <p>The assumption is that if you define a callback for a behavior,
%%      your generic module (where the callback is defined) should call
%%      that function at some point, using the implementation provided
%%      by the specific module (the one that implements the behavior).</p>
%%      <p>To avoid this warning, remove the unused callback definition.</p>
-module(unused_callbacks).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result
     || {File, AST} <- FilesAndASTs,
        Result <- analyze_file(File, AST)].

analyze_file(File, AST) ->
    CbNodes = [Node || Node <- AST, hank_utils:node_has_attrs(Node, [callback])],
    Callbacks = lists:map(fun(CbNode) ->
        % @TODO WIP!
        % Args = erl_syntax:attribute_arguments(CbNode),
        % FormList = erl_syntax:form_list(Args),
        % ct:print("ARGS: ~p", [Args]),
        % ct:print("FORMLIST: ~p", [FormList]),
        % ct:print("FORMELEMS: ~p", [erl_syntax:form_list_elements(FormList)]),
        {File, hank_utils:node_line(CbNode), hank_utils:attr_name(CbNode)}
    end, CbNodes),
    ct:print("CALLBACKS: ~p", [Callbacks]),
    analyze_callbacks(AST, Callbacks).

analyze_callbacks(_AST, []) ->
    []; %% Skip files with no callback definitions
analyze_callbacks(AST, Callbacks) ->
    Functions = [Node || Node <- AST, erl_syntax:type(Node) == function],
    [Result || {File, Line, Callback} <- Callbacks,
      Result <- analyze_callback(File, Line, Callback, Functions)].

analyze_callback(File, Line, Callback, Functions) ->
    case lists:any(fun(Function) -> function_has_atom(Function, Callback) end, Functions) of
      true -> [];
      false -> [set_result(File, Line, Callback)]
    end.

function_has_atom(FunctionNode, Atom) ->
    FuncAtoms = hank_utils:node_atoms(FunctionNode),
    ct:print("FUNCTION ATOMS: ~p", [FuncAtoms]),
    lists:any(fun(FuncAtom) -> FuncAtom =:= Atom end, FuncAtoms).

set_result(File, Line, Callback) ->
    #{file => File,
      line => Line,
      text => hank_utils:format_text("Callback ~p is not used anywhere in the module", [Callback])}.
