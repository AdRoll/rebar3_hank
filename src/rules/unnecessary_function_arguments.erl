%% @doc A rule to detect unnecessary function arguments.
%%      <p>The rule emits a warning for each function argument that is consistently
%%      ignored in all function clauses.</p>
%%      <p>To avoid this warning, remove the unused argument(s).</p>
%%      <p><b>Note:</b> This rule will not emit a warning if the function
%%      implements a NIF call.</p>
-module(unnecessary_function_arguments).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% To allow erl_syntax:syntaxTree/0 type spec
-elvis([{elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}},
        {elvis_style, invalid_dynamic_call, disable},
        {elvis_style, nesting_level, [6]}]).

-type imp_callbacks() :: #{File :: string() => [tuple()] | ignore}.

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    ImpCallbacks = callback_usage(FilesAndASTs),
    [Result
     || {File, AST} <- FilesAndASTs,
        not hank_utils:is_old_test_suite(File),
        not is_unrecognized_behaviour(File, ImpCallbacks),
        Node <- AST,
        erl_syntax:type(Node) == function,
        is_not_a_callback(ImpCallbacks, File, Node),
        Result <- analyze_function(File, Node)].

%% @doc Constructs a map with the callbacks of all the files.
%% 1. collect all the behaviors that the file implements.
%% 2. for each one of them evaluate with BehaviourMod:behaviour_info(callbacks)
%%    and keep the tuples in a single list.
%%    2.1. if that call fails, look for the module/s in all the files.
%%    2.2. if the module is not there, ignore the whole file
%%         (we can't be sure which functions are callbacks).
%%    2.3. If the module is there collect all callback attributes and add them
%%         to the list obtained in the item 2.
-spec callback_usage(hank_rule:asts()) -> imp_callbacks().
callback_usage(FilesAndASTs) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, FileCallbacks) ->
                          case hank_utils:node_has_attrs(Node, [behaviour, behavior]) of
                              true ->
                                  {_, _, _, {attribute, _, [{_, _, _, BehaviourMod}]}} = Node,
                                  Callbacks =
                                      try
                                          %% For pre-compiled behaviours like OTP ones
                                          BehaviourMod:behaviour_info(callbacks)
                                      catch
                                          %% Local behaviours will raise an exception
                                          _:_ ->
                                              case not_compiled_behaviour_callbacks(BehaviourMod,
                                                                                    FilesAndASTs)
                                              of
                                                  ignore ->
                                                      %% This is a way to break the foldl.
                                                      %% If some behaviour cannot be readed,
                                                      %% ignore the whole file
                                                      throw(ignore);
                                                  Callbacks0 ->
                                                      Callbacks0
                                              end
                                      end,
                                  FileCallbacks ++ Callbacks;
                              _ ->
                                  FileCallbacks
                          end
                       end,
                   ResultsForFile =
                       try
                           erl_syntax_lib:fold(FoldFun, [], erl_syntax:form_list(AST))
                       catch
                           ignore ->
                               ignore
                       end,
                   maps:put(File, ResultsForFile, Result)
                end,
                #{},
                FilesAndASTs).

%% @doc Returns the callbacks of a given behaviour module reading its nodes.
%%      If the module is not found, returns ignore.
-spec not_compiled_behaviour_callbacks(atom(), hank_rule:asts()) ->
                                          ignore | [{atom(), pos_integer()}].
not_compiled_behaviour_callbacks(BehaviourMod, FilesAndASTs) ->
    BehaviourFile = atom_to_list(BehaviourMod) ++ ".erl",
    case lists:keyfind(BehaviourFile, 1, FilesAndASTs) of
        {_, AST} ->
            CbNodes = [Node || Node <- AST, hank_utils:node_has_attrs(Node, [callback])],
            lists:map(fun(CbNode) ->
                         [DataArgs | _] = erl_syntax:attribute_arguments(CbNode),
                         [DataTuple | _] = erl_syntax:tuple_elements(DataArgs),
                         [Name, Arity] = erl_syntax:tuple_elements(DataTuple),
                         {erl_syntax:atom_value(Name), erl_syntax:integer_value(Arity)}
                      end,
                      CbNodes);
        false ->
            ignore
    end.

%% @doc Returns true if the file is a unrecongized_behaviour
-spec is_unrecognized_behaviour(string(), imp_callbacks()) -> boolean().
is_unrecognized_behaviour(File, ImpCallbacks) ->
    maps:get(File, ImpCallbacks, []) =:= ignore.

%% @doc It will check if arguments are ignored in all function clauses:
%%      [(_a, b, _c), (_x, b, c)]
%%      [[1, 0, 1], [1, 0, 0]] => [1, 0, 0] => warning 1st param!
%%      [(a, _b, c), (_, b, c)]
%%      [[0, 1, 0], [1, 0, 0]] => [0, 0, 0] => ok
analyze_function(File, Function) ->
    lists:foldl(fun(Result, Acc) ->
                   case set_result(File, Result) of
                       ok ->
                           Acc;
                       Error ->
                           [Error | Acc]
                   end
                end,
                [],
                check_function(Function)).

set_result(File, {error, Line, Text, IgnorePattern}) ->
    #{file => File,
      line => Line,
      text => Text,
      pattern => IgnorePattern};
set_result(_File, _) ->
    ok.

check_function(FunctionNode) ->
    Clauses = erl_syntax:function_clauses(FunctionNode),
    ComputedResults =
        lists:foldl(fun(Clause, Result) ->
                       case is_clause_a_nif_stub(Clause) of
                           true ->
                               Result; %% Discard NIF stubs!
                           false ->
                               Patterns = erl_syntax:clause_patterns(Clause),
                               ClausePatterns =
                                   [pattern_to_integer(Pattern) || Pattern <- Patterns],
                               check_unused_args(Result, ClausePatterns)
                       end
                    end,
                    [],
                    Clauses),
    check_computed_results(FunctionNode, ComputedResults).

%% @doc Checks if the last expression in a clause body applies erlang:nif_error/x
is_clause_a_nif_stub(Clause) ->
    LastClauseBodyNode =
        lists:last(
            erl_syntax:clause_body(Clause)),
    case hank_utils:application_node_to_mfa(LastClauseBodyNode) of
        {"erlang", "nif_error", _Args} ->
            true;
        _ ->
            false
    end.

%% @doc Checks if the given function node is callback of a behaviour that the file implements
-spec is_not_a_callback(imp_callbacks(), string(), erl_syntax:syntaxTree()) -> boolean().
is_not_a_callback(ImpCallbacks, File, FunctionNode) ->
    case maps:get(File, ImpCallbacks, []) of
        Callbacks when is_list(Callbacks) ->
            not
                lists:member(
                    hank_utils:function_tuple(FunctionNode), Callbacks);
        _ ->
            true
    end.

%% @doc Computes position by position (multiply/and)
%%      Will be 1 only when an argument is unused over all the function clauses
check_unused_args([], Arguments) ->
    Arguments;
check_unused_args(Result, Arguments) ->
    lists:zipwith(fun(A, B) -> A * B end, Result, Arguments).

pattern_to_integer({var, _Line, ArgNameAtom}) ->
    is_arg_ignored(atom_to_list(ArgNameAtom));
pattern_to_integer(_) ->
    0.

is_arg_ignored("_") ->
    1;
is_arg_ignored("_" ++ _) ->
    1;
is_arg_ignored(_) ->
    0.

check_computed_results(FunctionNode, Results) ->
    {_, Errors} =
        lists:foldl(fun(Result, {ArgNum, Errors}) ->
                       NewErrors =
                           case Result of
                               0 ->
                                   Errors;
                               1 ->
                                   [set_error(FunctionNode, ArgNum) | Errors]
                           end,
                       {ArgNum + 1, NewErrors}
                    end,
                    {1, []},
                    Results),
    Errors.

set_error(FuncNode, ArgNum) ->
    Line = hank_utils:node_line(FuncNode),
    FuncDesc = hank_utils:function_description(FuncNode),
    Text = hank_utils:format_text("~ts doesn't need its #~p argument", [FuncDesc, ArgNum]),
    FuncName = hank_utils:function_name(FuncNode),
    IgnorePattern = {list_to_atom(FuncName), erl_syntax:function_arity(FuncNode), ArgNum},
    {error, Line, Text, IgnorePattern}.

%% @doc Rule ignore specifications. Example:
%%      <pre>
%%      -hank([{unnecessary_function_arguments,
%%               %% You can give a list of multiple specs or a single one
%%               [%% Will ignore any unused argument from ignore_me/2 within the module
%%                {ignore_me, 2},
%%                %% Will ignore the 2nd argument from ignore_me_too/3 within the module
%%                {ignore_me_too, 3, 2},
%%                %% Will ignore any unused argument from any ignore_me_again/x
%%                %% within the module (no matter the function arity)
%%                ignore_me_again]}]).
%%      </pre>
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored(Pattern, Pattern) ->
    true;
ignored({FuncName, _, _}, FuncName) ->
    true;
ignored({FuncName, FuncArity, _}, {FuncName, FuncArity}) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.
