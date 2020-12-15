%%% @doc Utility functions
-module(hank_utils).

-export([attribute_name/1]).

%% @doc macro dodging version of erl_syntax:attribute_name/1
-spec attribute_name(erl_syntax:syntaxTree()) -> atom().
attribute_name(Node) ->
    N = erl_syntax:attribute_name(Node),
    try
        erl_syntax:concrete(N)
    catch
        _:_ ->
            N
    end.
