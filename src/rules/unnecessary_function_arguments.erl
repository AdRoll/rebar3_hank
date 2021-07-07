%% @doc A rule to detect unnecessary function arguments.
%%      <p>The rule emits a warning for each function argument that is consistently
%%      ignored in all function clauses.</p>
%%      <p>To avoid this warning, remove the unused argument(s).</p>
%%      <h3>Note</h3>
%%      <blockquote>
%%      This rule will not emit a warning if the function
%%      implements a NIF call (assuming that the stub function calls
%%      <code>erlang:nif_error/1,2</code>) or if it's a behaviour callback.
%%      In particular, the rule will not emit a warning for any exported
%%      function in modules that implement non-OTP behaviors or OTP behaviors
%%      that have dynamic callbacks, like <code>gen_statem</code> or <code>ct_suite</code>.
%%      </blockquote>
-module(unnecessary_function_arguments).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% Known OTP behaviours which do not implement dynamic callbacks like ct_suite.
-define(KNOWN_BEHAVIOURS,
        [application,
         gen_event,
         gen_server,
         ssh_channel,
         ssh_client_channel,
         ssh_client_key_api,
         ssh_server_channel,
         ssh_server_key_api,
         ssl_crl_cache_api,
         ssl_session_cache_api,
         supervisor,
         supervisor_bridge,
         tftp]).

%% Allow erl_syntax:syntaxTree/0 type spec
%% Allow Module:behaviour_info/1 call
-elvis([{elvis_style, invalid_dynamic_call, disable},
        {elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}}]).

-type imp_callbacks() :: #{File :: string() => [tuple()] | syntax_error}.

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    ImpCallbacks = callback_usage(FilesAndASTs),
    [Result
     || {File, AST} <- FilesAndASTs,
        not hank_utils:is_old_test_suite(File),
        is_parseable(File, ImpCallbacks),
        Node <- AST,
        erl_syntax:type(Node) == function,
        not is_callback(Node, File, ImpCallbacks),
        Result <- analyze_function(File, Node)].

%% @doc Constructs a map with the callbacks of all the files.
%% 1. collect all the behaviors that the file implements.
%% 2. for each one of them, build the list of their possible callbacks.
-spec callback_usage(hank_rule:asts()) -> imp_callbacks().
callback_usage(FilesAndASTs) ->
    lists:foldl(fun({File, AST}, Result) ->
                   FoldFun =
                       fun(Node, FileCallbacks) ->
                          case hank_utils:node_has_attrs(Node, [behaviour, behavior]) of
                              true ->
                                  FileCallbacks ++ behaviour_callbacks(Node, AST);
                              _ ->
                                  FileCallbacks
                          end
                       end,
                   ResultsForFile =
                       try
                           erl_syntax_lib:fold(FoldFun, [], erl_syntax:form_list(AST))
                       catch
                           syntax_error ->
                               syntax_error
                       end,
                   maps:put(File, ResultsForFile, Result)
                end,
                #{},
                FilesAndASTs).

%% @doc Returns the behaviour's callback list if the given behaviour Node is a "known behaviour",
%%      this means it is an OTP behaviour without "dynamic" callbacks.
%%      If this is not satisfied or the behaviour attribute contains a macro,
%%      this function returns the whole list of functions that exported from the file.
%%      That's because, for dynamic behaviors, any exported function can be the implementation
%%      of a callback.
-spec behaviour_callbacks(erl_syntax:syntaxTree(), erl_syntax:forms()) ->
                             [{atom(), non_neg_integer()}].
behaviour_callbacks(Node, AST) ->
    try erl_syntax_lib:analyze_wild_attribute(Node) of
        {_, BehaviourMod} ->
            case lists:member(BehaviourMod, ?KNOWN_BEHAVIOURS) of
                true ->
                    BehaviourMod:behaviour_info(callbacks);
                false ->
                    module_exports(AST)
            end
    catch
        _:syntax_error ->
            %% There is a macro, then return all its exports, just in case
            module_exports(AST)
    end.

-spec module_exports(erl_syntax:forms()) -> [{atom(), non_neg_integer()}].
module_exports(AST) ->
    FoldFun =
        fun(Node, Exports) ->
           case erl_syntax:type(Node) of
               attribute ->
                   try erl_syntax_lib:analyze_attribute(Node) of
                       {export, NewExports} ->
                           Exports ++ NewExports;
                       _ ->
                           Exports
                   catch
                       _:syntax_error ->
                           %% Probably macros, we can't parse this module
                           throw(syntax_error)
                   end;
               _ ->
                   Exports
           end
        end,
    erl_syntax_lib:fold(FoldFun, [], erl_syntax:form_list(AST)).

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

%% @doc Checks if the given function node implements a callback
-spec is_callback(erl_syntax:syntaxTree(), string(), imp_callbacks()) -> boolean().
is_callback(FunctionNode, File, ImpCallbacks) ->
    lists:member(
        hank_utils:function_tuple(FunctionNode), maps:get(File, ImpCallbacks, [])).

%% @doc Returns true if hank could parse the file.
%%      Otherwise the file is ignored and no warnings are reported for it
-spec is_parseable(string(), imp_callbacks()) -> boolean().
is_parseable(File, ImpCallbacks) ->
    maps:get(File, ImpCallbacks, []) =/= syntax_error.

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
