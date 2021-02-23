%% @doc A rule to detect unneded function parameters.
%%      <p>The rule emits a warning for each function parameter that is consistently
%%      ignored in all function clauses.</p>
%%      <p>To avoid this warning, remove the unused parameter(s).</p>
%%      <p><b>Note:</b> This rule will not emit a warning if the function
%%      implements a behaviour callback or a NIF call.</p>
-module(unnecessary_function_arguments).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [Result
     || {File, AST} <- FilesAndASTs,
        not hank_utils:implements_behaviour(AST),
        Node <- AST,
        erl_syntax:type(Node) == function,
        Result <- analyze_function(File, Node)].

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

set_result(File, {error, Line, Text}) ->
    #{file => File,
      line => Line,
      text => Text};
set_result(_File, _) ->
    ok.

check_function(FunctionNode) ->
    Line = hank_utils:node_line(FunctionNode),
    FuncDesc = hank_utils:function_description(FunctionNode),
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
    check_computed_results(FuncDesc, Line, ComputedResults).

%% @doc Checks if the last expression in a clause body applies `erlang:nif_error/x`
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

check_computed_results(FuncDesc, Line, Results) ->
    {_, Errors} =
        lists:foldl(fun(Result, {Pos, Errors}) ->
                       NewErrors =
                           case Result of
                               0 ->
                                   Errors;
                               1 ->
                                   [set_error(Line, Pos, FuncDesc) | Errors]
                           end,
                       {Pos + 1, NewErrors}
                    end,
                    {1, []},
                    Results),
    Errors.

set_error(Line, Pos, FuncDesc) ->
    Text = hank_utils:format_text("~ts doesn't need its #~p argument", [FuncDesc, Pos]),
    {error, Line, Text}.
