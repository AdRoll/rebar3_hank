%% @doc A rule to detect unused callbacks.
%%      <p>This rule will check all callbacks defined in a module and find
%%      those that are not used anywhere in the module itself.</p>
%%      <p>It will emit a warning if it can't find the callback's atom name
%%      being used anywhere within a module. It will NOT emit a warning if an atom
%%      named as a callback is being used, no matter for what that atom is used.</p>
%%      <p>This limitation is due to the fact that there are many ways to call
%%      a function in Erlang (particularly when dynamic calls are involved).</p>
%%      <p>The assumption is that if you define a callback for a behavior,
%%      your generic module (where the callback is defined) should call
%%      that function at some point, using the implementation provided
%%      by the specific module (the one that implements the behavior).</p>
%%      <p>To avoid this warning, remove the unused callback definition.</p>
-module(unused_callbacks).

-behaviour(hank_rule).

%% @TODO [#81 + #82] Correctly handle macros
-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result || {File, AST} <- FilesAndASTs, Result <- analyze_file(File, AST)].

analyze_file(File, AST) ->
    CbNodes = [Node || Node <- AST, hank_utils:node_has_attrs(Node, [callback])],
    Callbacks =
        lists:map(fun(CbNode) ->
                     [CbDataArgs | _] = erl_syntax:attribute_arguments(CbNode),
                     [CbDataTuple | _] = erl_syntax:tuple_elements(CbDataArgs),
                     [CbName, CBArit] = erl_syntax:tuple_elements(CbDataTuple),
                     {File,
                      hank_utils:node_line(CbNode),
                      erl_syntax:atom_value(CbName),
                      erl_syntax:integer_value(CBArit)}
                  end,
                  CbNodes),
    analyze_callbacks(AST, Callbacks).

analyze_callbacks(_AST, []) ->
    []; %% Skip files with no callback definitions
analyze_callbacks(AST, Callbacks) ->
    Functions = [Node || Node <- AST, erl_syntax:type(Node) == function],
    [set_result(File, Line, Callback, Arity)
     || {File, Line, Callback, Arity} <- Callbacks, not is_used_callback(Callback, Functions)].

is_used_callback(Callback, Functions) ->
    lists:any(fun(Function) -> function_has_atom(Function, Callback) end, Functions).

function_has_atom(FuncNode, FuncName) ->
    FuncBodies =
        [Body
         || Clause <- erl_syntax:function_clauses(FuncNode),
            Body <- erl_syntax:clause_body(Clause)],
    FuncAtoms = hank_utils:node_atoms(FuncBodies),
    lists:any(fun(FuncAtom) -> FuncAtom =:= FuncName end, FuncAtoms).

set_result(File, Line, Callback, Arity) ->
    #{file => File,
      line => Line,
      text =>
          hank_utils:format_text("Callback ~tw/~B is not used anywhere in the module",
                                 [Callback, Arity])}.
