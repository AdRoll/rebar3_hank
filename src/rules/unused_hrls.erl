-module(unused_hrls).

-behaviour(hank_rule).

-export([analyze/1]).

-spec analyze(hank_rule:asts()) -> [hank_rule:result()].
analyze(ASTs) ->
    IncludePaths =
        [IncludePath || {File, AST} <- ASTs, IncludePath <- inlcude_paths(File, AST)],
    IncludeLibPaths =
        [hank_utils:expand_lib_dir(IncludeLibPath)
         || {_, AST} <- ASTs, IncludeLibPath <- inlcude_lib_paths(AST)],
    ct:pal("IncludeLibPaths = ~p", [IncludeLibPaths]),
    [#{file => File,
       line => 0,
       text => "This file is unused"}
     || {File, _} <- ASTs,
        filename:extension(File) == ".hrl",
        is_unused_local(File, IncludePaths),
        is_unused_lib(File, IncludeLibPaths)].

inlcude_paths(File, AST) ->
    [{File, erl_syntax:concrete(IncludedFile)}
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        % Yeah, include_lib can also be used as include ¯\_(ツ)_/¯ (check epp's code)
        hank_utils:attribute_name(Node) == include
        orelse hank_utils:attribute_name(Node) == include_lib,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

inlcude_lib_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        hank_utils:attribute_name(Node) == include_lib,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

is_unused_local(FilePath, IncludePaths) ->
    not
        lists:any(fun({ErlPath, IncludePath}) -> matches(FilePath, ErlPath, IncludePath) end,
                  IncludePaths).

matches(IncludePath, _, IncludePath) ->
    % The path used in the include directive is exactly the file path
    true;
matches(FilePath, ErlPath, FullIncludePath) ->
    % We remove relative paths because FilePath will not be a relative path and,
    % in any case, the paths will be relative to something that we don't know.
    IncludePath =
        unicode:characters_to_list(
            string:replace(
                string:replace(FullIncludePath, "../", "", all), "./", "", all)),
    case string:find(FilePath, IncludePath, trailing) of
        IncludePath ->
            % FilePath ends with IncludePath, we keep the rest
            FileDir =
                string:reverse(
                    string:replace(
                        string:reverse(FilePath), string:reverse(IncludePath), "")),
            % Then we check that the module lives in the same place as the header
            string:find(ErlPath, FileDir, leading) /= nomatch;
        _ ->
            % The include path doesn't end with the same file path
            false
    end.

%% @todo implement!
is_unused_lib(_File, _IncludeLibPaths) ->
    true.
