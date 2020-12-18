%%% @doc Utility functions
-module(hank_utils).

%% To allow erl_syntax:syntaxTree
-elvis([{elvis_style, atom_naming_convention, #{regex => "^([a-zA-Z][a-z0-9]*_?)*$"}}]).

-export([attribute_name/1, paths_match/2]).

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

%% @doc Verifies if FilePath and IncludePath refer both to the same file.
%%      Note that we can't just compare both filename:absname's here, since we
%%      don't really know what is the absolute path of the file referred by
%%      the include directive.
-spec paths_match(string(), string()) -> boolean().
paths_match(IncludePath, IncludePath) ->
    % The path used in the include directive is exactly the file path
    true;
paths_match(FilePath, IncludePath) ->
    % We remove relative paths because FilePath will not be a relative path and,
    % in any case, the paths will be relative to something that we don't know.
    CleanFilePath = clean_path(FilePath),
    CleanIncludePath = clean_path(IncludePath),
    % Note that this might result in some false negatives.
    % For instance, Hank may think that lib/app1/include/header.hrl is used
    % if lib/app2/src/module.erl contains -include("header.hrl").
    % when, in reality, module is including lib/app2/include/header.erl
    % That should be an extremely edge scenario and Hank never promised to find
    % ALL the dead code, anyway. It just promised that *if* it finds something,
    % that's dead code, 100% sure.
    compare_paths(CleanIncludePath, CleanFilePath).

clean_path(Path) ->
    unicode:characters_to_list(
        string:replace(
            string:replace(Path, "../", "", all), "./", "", all)).

compare_paths({PathA, LenA}, {PathB, LenB}) when LenA > LenB ->
    PathB == string:find(PathA, PathB, trailing);
compare_paths({PathA, _}, {PathB, _}) ->
    PathA == string:find(PathB, PathA, trailing);
compare_paths(PathA, PathB) ->
    compare_paths({PathA, length(PathA)}, {PathB, length(PathB)}).
