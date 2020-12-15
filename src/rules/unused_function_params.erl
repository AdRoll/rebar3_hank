-module(unused_function_params).

-behaviour(hank_rule).

-export([analyze/2]).

-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(ASTs, _Context) ->
    lists:foldl(fun({File, AST}, Acc) ->
                   FunctionsTrees = extract_functions_from_ast(AST),
                   ct:print("~p~n", [FunctionsTrees]),
                   Acc ++ check_functions(FunctionsTrees)
                end,
                [],
                ASTs).

check_functions(FunctionsTrees) ->
    [check_function(FunctionTree) || FunctionTree <- FunctionsTrees].

check_function(FunctionTree) ->
    ClauseTrees = erl_syntax:function_clauses(FunctionTree),
    % [{tree,clause,
    %    {attr,[{text,"multi_fun"},{location,10}],[],none},
    %    {clause,[{atom,[{text,"undefined"},{location,10}],undefined},
    %             {var,[{text,"_Arg2"},{location,10}],'_Arg2'}],
    %            none,
    %            [{atom,[{text,"ok"},{location,11}],ok}]}},
    % [(_a, b, _c), (_x, b, _c)]
    % [{1, 0, 1}, {1, 0, 1}] warning!
    %
    % [(a, _b, c), (_x, b, c)]
    % [{0, 1, 0}, {1, 0, 0}] ok
    ArgsList =
        lists:foldl(fun({tree, clause, TreeClause}, Acc) ->
                       {_, {clause, FunctionArgs, _, _}} = TreeClause,
                       Args = map_args(FunctionArgs),
                       Acc ++ Args
                    end,
                    [],
                    ClauseTrees),
    case check_args(ArgsList) of
        {error, Error} ->
            #{line => 23, message => Error};
        _ ->
            ok
    end.

map_args(FunctionArgs) ->
    % TODO!
    ok.

check_args(ArgsList) ->
    % TODO!
    ok.

extract_functions_from_ast(AST) ->
    [Tree || Tree <- AST, is_function_tree(Tree)].

is_function_tree({tree, function, _, _}) ->
    true;
is_function_tree(_) ->
    false.
