-module(unnecessary_function_arguments).
-moduledoc """
A rule to detect unnecessary function arguments.

The rule emits a warning for each function argument that is consistently
ignored in all function clauses.

To avoid this warning, remove the unused argument(s).

> ## Note
> This rule will not emit a warning if the function
> implements a NIF call (assuming that the stub function calls
> `erlang:nif_error/1,2`) or if it's a behaviour callback.
> In particular, the rule will not emit a warning for any exported
> function in modules that implement non-OTP behaviors or OTP behaviors
> that have dynamic callbacks, like `gen_statem` or `ct_suite`.
> A behaviour declared in a header file that the module includes (directly or
> through other header files) counts as implemented by the module, as long as
> that header file is part of the analyzed project: headers from dependencies
> are not inspected. Since a header file can't always be told apart from
> another one with the same name, a behaviour declared in any of them counts.
> And if the path of an include is not literal text (e.g. a macro), a
> behaviour declared in any of the project's header files counts.
> It will also not emit a warning if the function is "known"
> even if not in a behaviour, like `parse_transform/2`.
""".

%% Throw is used correctly in this module as a nonlocal return within a fold function
-elvis([{elvis_style, no_throw, disable}]).
%% We use Mod:behaviour_info/1 to get the callbacks from behaviour-defining modules
-elvis([{elvis_style, no_invalid_dynamic_calls, disable}]).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

%% Allow erl_syntax:syntaxTree/0 type spec
%% Allow Module:behaviour_info/1 call
-elvis([
    {elvis_style, invalid_dynamic_call, disable},
    {elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}}
]).

-type imp_callbacks() :: #{File :: string() => [tuple()] | syntax_error}.

-doc false.
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, Context) ->
    ImpCallbacks = callback_usage(FilesAndASTs, Context),
    [
        Result
     || {File, AST} <- FilesAndASTs,
        not hank_utils:is_old_test_suite(File),
        is_parseable(File, ImpCallbacks),
        Node <- AST,
        erl_syntax:type(Node) =:= function,
        not is_exception_fun(hank_utils:function_tuple(Node)),
        not is_callback(Node, File, ImpCallbacks),
        Result <- analyze_function(File, Node)
    ].

-doc """
Constructs a map with the callbacks of all the files.

1. collect all the behaviors that the file implements, either declared in the
   file itself or in any header file it includes (directly or transitively).
2. for each one of them, build the list of their possible callbacks.
3. if that list could not be built (usually because of macros), add `syntax_error` instead.
""".
-spec callback_usage(hank_rule:asts(), hank_context:t()) -> imp_callbacks().
callback_usage(FilesAndASTs, Context) ->
    IncludedBehaviours = included_behaviours(FilesAndASTs, Context),
    lists:foldl(
        fun({File, AST}, Result) ->
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
                    erl_syntax_lib:fold(
                        FoldFun,
                        [],
                        erl_syntax:form_list(AST ++ maps:get(File, IncludedBehaviours, []))
                    )
                catch
                    syntax_error ->
                        syntax_error
                end,
            maps:put(File, ResultsForFile, Result)
        end,
        #{},
        FilesAndASTs
    ).

-doc """
Maps each file to the behaviour attributes declared in the analyzed header files that
it includes, directly or through other header files. Files that include none of them
are left out.
""".
-spec included_behaviours(hank_rule:asts(), hank_context:t()) ->
    #{file:filename() => [erl_syntax:syntaxTree()]}.
included_behaviours(FilesAndASTs, Context) ->
    Headers = [File || {File, _AST} <- FilesAndASTs, filename:extension(File) =:= ".hrl"],
    %% Keyed by their known behaviour module (undefined for any other behaviour, whose
    %% callbacks are any exported function), to compute their callbacks once per module
    HeaderBehaviours =
        #{
            Header => Behaviours
         || {Header, AST} <- FilesAndASTs,
            filename:extension(Header) =:= ".hrl",
            Behaviours <- [
                #{
                    known_behaviour(Node) => Node
                 || Node <- AST, hank_utils:node_has_attrs(Node, [behaviour, behavior])
                }
            ],
            map_size(Behaviours) > 0
        },
    case map_size(HeaderBehaviours) of
        0 ->
            %% No header file declares a behaviour, so there is nothing to look for
            #{};
        _ ->
            Includes = included_headers(FilesAndASTs, Headers, Context),
            Reached = reached_behaviours(Headers, Includes, HeaderBehaviours),
            #{
                File => maps:values(Behaviours)
             || File := Direct <- Includes,
                Behaviours <- [
                    #{
                        Key => Node
                     || Header <- Direct,
                        Key := Node <- maps:get(Header, Reached, #{})
                    }
                ],
                map_size(Behaviours) > 0
            }
    end.

-doc """
Maps each header file to the behaviours declared in the header files (itself included)
that it reaches through `Includes`.

For each header file declaring a behaviour, the header files that reach it are found by
walking the include graph backwards from it, which also copes with include cycles.
""".
reached_behaviours(Headers, Includes, HeaderBehaviours) ->
    IncludedBy =
        maps:groups_from_list(
            fun({Included, _Header}) -> Included end,
            fun({_Included, Header}) -> Header end,
            [
                {Included, Header}
             || Header <- Headers, Included <- maps:get(Header, Includes)
            ]
        ),
    maps:map(
        fun(_Header, BehaviourHeaders) ->
            #{
                Key => Node
             || BehaviourHeader <- BehaviourHeaders,
                Key := Node <- maps:get(BehaviourHeader, HeaderBehaviours)
            }
        end,
        maps:groups_from_list(
            fun({Header, _Behaviour}) -> Header end,
            fun({_Header, Behaviour}) -> Behaviour end,
            [
                {Header, Behaviour}
             || Behaviour := _ <- HeaderBehaviours,
                Header := _ <- reaching([Behaviour], IncludedBy, #{Behaviour => true})
            ]
        )
    ).

-doc """
Maps each file to the header files (among `Headers`) that it includes directly, through
`-include` or `-include_lib`, matched like `unused_hrls` does. If the path of an include
is not literal text (e.g. a macro), it may be any of them.
""".
-spec included_headers(hank_rule:asts(), [file:filename()], hank_context:t()) ->
    #{file:filename() => [file:filename()]}.
included_headers(FilesAndASTs, Headers, Context) ->
    FileIncludes = [{File, include_paths(AST, Context)} || {File, AST} <- FilesAndASTs],
    HeaderNames =
        [{Header, filename:absname(Header), reversed_name(Header)} || Header <- Headers],
    %% Each include path is matched against the header files just once
    Matches =
        #{
            IncludePath => [
                Header
             || {Header, _AbsHeader, _ReversedName} = HeaderName <- HeaderNames,
                includes(IncludePath, HeaderName)
            ]
         || IncludePath <- lists:usort([P || {_File, Ps} <- FileIncludes, P <- Ps])
        },
    #{
        File => lists:usort([Header || P <- Ps, Header <- maps:get(P, Matches)])
     || {File, Ps} <- FileIncludes
    }.

include_paths(AST, Context) ->
    case hank_utils:has_non_literal_attr_args(AST, [include, include_lib]) of
        true ->
            [any];
        false ->
            [
                {include, Path, reversed_name(Path)}
             || Path <- hank_utils:attr_args_concrete(AST, include)
            ] ++
                [
                    {include_lib, Path, reversed_name(Path),
                        hank_utils:expand_lib_dir(Path, Context)}
                 || Path <- hank_utils:attr_args_concrete(AST, include_lib)
                ]
    end.

includes(any, _HeaderName) ->
    true;
includes({include, Path, ReversedName}, {Header, _AbsHeader, ReversedHeaderName}) ->
    may_match(ReversedName, ReversedHeaderName) andalso hank_utils:paths_match(Path, Header);
includes({include_lib, Path, ReversedName, AbsPath}, {Header, AbsHeader, ReversedHeaderName}) ->
    %% include_lib can also be used as include (check epp's code)
    AbsPath =:= AbsHeader orelse
        may_match(ReversedName, ReversedHeaderName) andalso hank_utils:paths_match(Path, Header).

-doc """
Whether `hank_utils:paths_match/2` may match two paths, given their last components
(what follows their last `/`) reversed: it only matches paths when one of them ends with
the other once `"../"` and `"./"` are removed, which leaves their last components as they
are. This is much faster, so it spares most of its calls.
""".
may_match(ReversedName, ReversedHeaderName) ->
    lists:prefix(ReversedName, ReversedHeaderName) orelse
        lists:prefix(ReversedHeaderName, ReversedName).

reversed_name(Path) ->
    lists:takewhile(fun(Char) -> Char =/= $/ end, lists:reverse(unicode:characters_to_list(Path))).

-doc """
Walks the reversed include graph from the given header files, collecting every header
file that reaches them.
""".
reaching([], _IncludedBy, Reached) ->
    Reached;
reaching([Header | Headers], IncludedBy, Reached) ->
    New = [
        Includer
     || Includer <- maps:get(Header, IncludedBy, []),
        not is_map_key(Includer, Reached)
    ],
    reaching(New ++ Headers, IncludedBy, maps:merge(Reached, maps:from_keys(New, true))).

-doc """
Returns the behaviour's callback list if the given behaviour `Node` is a "known behaviour",
i.e., if it is an OTP behaviour without "dynamic" callbacks.
If this is not satisfied or the behaviour attribute contains a macro,
this function returns the whole list of functions that are exported from the file.
That's because, for dynamic behaviors, any exported function can be the implementation
of a callback.
""".
-spec behaviour_callbacks(erl_syntax:syntaxTree(), erl_syntax:forms()) ->
    [{atom(), non_neg_integer()}].
behaviour_callbacks(Node, AST) ->
    case known_behaviour(Node) of
        undefined ->
            module_exports(AST);
        BehaviourMod ->
            BehaviourMod:behaviour_info(callbacks)
    end.

-doc """
Returns the module of the given behaviour `Node` if it is a "known behaviour", or
`undefined` otherwise.
""".
known_behaviour(Node) ->
    KnownBehaviours = [
        application,
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
        tftp
    ],
    try erl_syntax_lib:analyze_wild_attribute(Node) of
        {_, BehaviourMod} ->
            case lists:member(BehaviourMod, KnownBehaviours) of
                true ->
                    BehaviourMod;
                false ->
                    undefined
            end
    catch
        _:syntax_error ->
            %% There is a macro, then return all its exports, just in case
            undefined
    end.

-spec module_exports(erl_syntax:forms()) -> [{atom(), non_neg_integer()}].
module_exports(AST) ->
    FoldFun =
        fun(Node, {ExportAll, Exports, Functions} = Acc) ->
            case erl_syntax:type(Node) of
                attribute ->
                    try erl_syntax_lib:analyze_attribute(Node) of
                        {export, NewExports} ->
                            {ExportAll, Exports ++ NewExports, Functions};
                        {compile, Opts} ->
                            {ExportAll orelse has_export_all(Opts), Exports, Functions};
                        _ ->
                            Acc
                    catch
                        _:syntax_error ->
                            %% Probably macros, we can't parse this module
                            throw(syntax_error)
                    end;
                function ->
                    Function = erl_syntax_lib:analyze_function(Node),
                    {ExportAll, Exports, [Function | Functions]};
                _ ->
                    Acc
            end
        end,
    case erl_syntax_lib:fold(FoldFun, {false, [], []}, erl_syntax:form_list(AST)) of
        {true, _Exports, Functions} ->
            Functions;
        {false, Exports, _Functions} ->
            Exports
    end.

has_export_all(List) when is_list(List) ->
    lists:member(export_all, List);
has_export_all(export_all) ->
    true;
has_export_all(_Opt) ->
    false.

-doc """
It will check if arguments are ignored in all function clauses:
```erlang
[(_a, b, _c), (_x, b, c)]
[[1, 0, 1], [1, 0, 0]] => [1, 0, 0] => "warning 1st param!"
[(a, _b, c), (_, b, c)]
[[0, 1, 0], [1, 0, 0]] => [0, 0, 0] => ok
```
""".
analyze_function(File, Function) ->
    lists:foldl(
        fun(Result, Acc) ->
            case set_result(File, Result) of
                ok ->
                    Acc;
                Error ->
                    [Error | Acc]
            end
        end,
        [],
        check_function(Function)
    ).

set_result(File, {error, Line, Text, IgnorePattern}) ->
    #{
        file => File,
        line => Line,
        text => Text,
        pattern => IgnorePattern
    };
set_result(_File, _) ->
    ok.

check_function(FunctionNode) ->
    Clauses = erl_syntax:function_clauses(FunctionNode),
    ComputedResults =
        lists:foldl(
            fun(Clause, Result) ->
                case is_clause_a_nif_stub(Clause) of
                    true ->
                        %% Discard NIF stubs!
                        Result;
                    false ->
                        Patterns = erl_syntax:clause_patterns(Clause),
                        ClausePatterns =
                            [pattern_to_integer(Pattern) || Pattern <- Patterns],
                        check_unused_args(Result, ClausePatterns)
                end
            end,
            [],
            Clauses
        ),
    check_computed_results(FunctionNode, ComputedResults).

-doc """
Checks if the last expression in a clause body applies `erlang:nif_error/?`.
""".
is_clause_a_nif_stub(Clause) ->
    LastClauseBodyNode =
        lists:last(
            erl_syntax:clause_body(Clause)
        ),
    case hank_utils:application_node_to_mfa(LastClauseBodyNode) of
        {"erlang", "nif_error", _Args} ->
            true;
        _ ->
            false
    end.

-doc """
Checks if the given function node implements a callback.
""".
-spec is_callback(erl_syntax:syntaxTree(), string(), imp_callbacks()) -> boolean().
is_callback(FunctionNode, File, ImpCallbacks) ->
    lists:member(
        hank_utils:function_tuple(FunctionNode), maps:get(File, ImpCallbacks, [])
    ).

-doc """
Allows exceptions for functions whose name and arity are known but are
not associated with a given behaviour (e.g. `parse_transform/2`).
""".
is_exception_fun({parse_transform, 2}) ->
    true;
is_exception_fun(_) ->
    false.

-doc """
Returns true if Hank could parse the file.
Otherwise the file is ignored and no warnings are reported for it.
""".
-spec is_parseable(string(), imp_callbacks()) -> boolean().
is_parseable(File, ImpCallbacks) ->
    maps:get(File, ImpCallbacks, []) =/= syntax_error.

-doc """
Computes position by position (multiply/and).
Will be 1 only when an argument is unused over all the function clauses.
""".
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
        lists:foldl(
            fun(Result, {ArgNum, Errors}) ->
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
            Results
        ),
    Errors.

set_error(FuncNode, ArgNum) ->
    Line = hank_utils:node_line(FuncNode),
    FuncDesc = hank_utils:function_description(FuncNode),
    Text = hank_utils:format_text("~ts doesn't need its #~p argument", [FuncDesc, ArgNum]),
    FuncName = hank_utils:function_name(FuncNode),
    IgnorePattern = {list_to_atom(FuncName), erl_syntax:function_arity(FuncNode), ArgNum},
    {error, Line, Text, IgnorePattern}.

-doc """
Rule ignore specifications. Example:
```erlang
-hank([{unnecessary_function_arguments,
         %% You can give a list of multiple specs or a single one
         [%% Will ignore any unused argument from ignore_me/2 within the module
          {ignore_me, 2},
          %% Will ignore the 2nd argument from ignore_me_too/3 within the module
          {ignore_me_too, 3, 2},
          %% Will ignore any unused argument from any ignore_me_again/x
          %% within the module (no matter the function arity)
          ignore_me_again]}]).
```
""".
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored(Pattern, Pattern) ->
    true;
ignored({FuncName, _, _}, FuncName) ->
    true;
ignored({FuncName, FuncArity, _}, {FuncName, FuncArity}) ->
    true;
ignored(_Pattern, _IgnoreSpec) ->
    false.
