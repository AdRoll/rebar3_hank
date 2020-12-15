%%% @doc Utility functions
-module(hank_utils).

-export([attribute_name/1]).
-export([expand_lib_dir/2]).

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

%% @doc Copied from epp:expand_lib_dir(Name).
expand_lib_dir(Name, _Context) ->
    try
        [App | Path] = filename:split(Name),
        LibDir = code:lib_dir(list_to_atom(App)),
        fname_join([LibDir | Path])
    catch
        _:_ ->
            rebar_api:warn("Lib dir not found: ~p", [Name]),
            filename:absname(Name)
    end.

%% @equiv epp:fname_join(Name).
fname_join(["." | [_ | _] = Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).
