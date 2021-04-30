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
%% @todo [#81 + #82] Correctly handle macros
-module(unused_callbacks).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

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
                     {hank_utils:node_line(CbNode),
                      erl_syntax:atom_value(CbName),
                      erl_syntax:integer_value(CBArit)}
                  end,
                  CbNodes),
    analyze_callbacks(File, AST, Callbacks).

analyze_callbacks(_File, _AST, []) ->
    []; %% Skip files with no callback definitions
analyze_callbacks(File, AST, Callbacks) ->
    [set_result(File, Line, Callback, Arity)
     || {Line, Callback, Arity} <- Callbacks, not is_used_callback(Callback, AST)].

is_used_callback(Callback, Nodes) ->
    lists:any(fun(Node) -> hank_utils:node_has_atom(Node, Callback) end, Nodes).

set_result(File, Line, Callback, Arity) ->
    #{file => File,
      line => Line,
      text =>
          hank_utils:format_text("Callback ~tw/~B is not used anywhere in the module",
                                 [Callback, Arity]),
      pattern => {Callback, Arity}}.

%% @doc Rule ignore specifications example:
%%      <pre>
%%      -hank([{unused_callbacks,
%%              [all, %% Will ignore all versions of the all callback (i.e. any arity)
%%               {just, 1} %% Will ignore just(term()) but not just() nor just(_, _) callbacks
%%              ]},
%%      </pre>
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored({Callback, Arity}, {Callback, Arity}) ->
    true;
ignored({Callback, _Arity}, Callback) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.
