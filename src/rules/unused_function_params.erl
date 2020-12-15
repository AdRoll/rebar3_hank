-module(unused_function_params).

-behaviour(hank_rule).

-export([analyze/2]).

-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(ASTs, _Context) ->
    [Result
     || {File, AST} <- ASTs,
        Function <- [Node || Node <- AST, erl_syntax:type(Node) == function],
        Result <- [set_result(File, check_function(Function))],
        Result =/= ok].

set_result(File, {error, Line, Text}) ->
    #{file => File,
      line => Line,
      text => Text};
set_result(_File, _) ->
    ok.

check_function(FunctionNode) ->
    % [{tree,clause,
    %    {attr,[{text,"multi_fun"},{location,10}],[],none},
    %    {clause,[{atom,[{text,"undefined"},{location,10}],undefined},
    %             {var,[{text,"_Arg2"},{location,10}],'_Arg2'}],
    %            none,
    %            [{atom,[{text,"ok"},{location,11}],ok}]}},
    % [(_a, b, _c), (_x, b, _c)]
    % [[1, 0, 1], [1, 0, 1]] warning!
    %
    % [(a, _b, c), (_x, b, c)]
    % [[0, 1, 0], [1, 0, 0]] ok
    Clauses = erl_syntax:function_clauses(FunctionNode),
    ct:print("CLAUSES: ~p", [Clauses]),
    Arguments =
        lists:map(fun(Clause) ->
                     Patterns = erl_syntax:clause_patterns(Clause),
                     ct:print("PATTERNS: ~p", [Patterns]),
                     lists:map(fun pattern_to_integer/1, Patterns)
                  end,
                  Clauses),
    check_args(Arguments).

pattern_to_integer({var, [{text, ArgName}, {location, _Line}], _}) ->
    is_arg_ignored(ArgName);
pattern_to_integer(_) ->
    0.

is_arg_ignored("_") ->
    1;
is_arg_ignored("_" ++ _) ->
    1;
is_arg_ignored(_) ->
    0.

check_args(Arguments) ->
    % @TODO WIP!
    %% {error, Line, Text}
    ct:print("ARGUMENT LIST: ~p", [Arguments]),
    ok.
