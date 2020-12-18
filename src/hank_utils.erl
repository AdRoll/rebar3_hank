%%% @doc Utility functions
-module(hank_utils).

%% To allow erl_syntax:syntaxTree
-elvis([{elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}}]).

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
