-module(unused_hrls).

-behaviour(hank_rule).

-export([analyze/2]).

-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, Context) ->
    {Files, ASTs} = lists:unzip(FilesAndASTs),
    IncludePaths = [IncludePath || AST <- ASTs, IncludePath <- inlcude_paths(AST)],
    IncludeLibPaths =
        [hank_utils:expand_lib_dir(IncludeLibPath, Context)
         || AST <- ASTs, IncludeLibPath <- inlcude_lib_paths(AST)],
    ct:pal("IncludePaths = ~p", [IncludePaths]),
    ct:pal("IncludeLibPaths = ~p", [IncludeLibPaths]),
    [#{file => File,
       line => 0,
       text => "This file is unused"}
     || File <- Files,
        filename:extension(File) == ".hrl",
        is_unused_local(File, IncludePaths),
        is_unused_lib(File, IncludeLibPaths)].

inlcude_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
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
    not lists:any(fun(IncludePath) -> matches(FilePath, IncludePath) end, IncludePaths).

matches(IncludePath, IncludePath) ->
    % The path used in the include directive is exactly the file path
    true;
matches(FilePath, FullIncludePath) ->
    % We remove relative paths because FilePath will not be a relative path and,
    % in any case, the paths will be relative to something that we don't know.
    IncludePath =
        unicode:characters_to_list(
            string:replace(
                string:replace(FullIncludePath, "../", "", all), "./", "", all)),
    % Note that this might result in some false negatives.
    % For instance, Hank may think that lib/app1/include/header.hrl is used
    % if lib/app2/src/module.erl contains -include("header.hrl").
    % when, in reality, module is including lib/app2/include/header.erl
    % That should be an extremely edge scenario and Hank never promised to find
    % ALL the dead code, anyway. It just promised that *if* it finds something,
    % that's dead code, 100% sure.
    IncludePath == string:find(FilePath, IncludePath, trailing).

is_unused_lib(File, IncludeLibPaths) ->
    ct:pal("File = ~p,\nIncludeLibPaths = ~p", [File, IncludeLibPaths]),
    true.
