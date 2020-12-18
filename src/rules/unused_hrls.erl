%% @doc A rule to detect unused header files.
%%      <p>To avoid this warning, remove the unused header files.</p>
%% @todo Figure out the absname of IncludePath
%%       [https://github.com/AdRoll/rebar3_hank/issues/31]
-module(unused_hrls).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, Context) ->
    {Files, ASTs} = lists:unzip(FilesAndASTs),
    IncludePaths = [IncludePath || AST <- ASTs, IncludePath <- include_paths(AST)],
    IncludeLibPaths =
        [expand_lib_dir(IncludeLibPath, Context)
         || AST <- ASTs, IncludeLibPath <- include_lib_paths(AST)],
    [#{file => File,
       line => 0,
       text => "This file is unused"}
     || File <- Files,
        filename:extension(File) == ".hrl",
        is_unused_local(File, IncludePaths),
        is_unused_lib(File, IncludeLibPaths)].

include_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        % Yeah, include_lib can also be used as include ¯\_(ツ)_/¯ (check epp's code)
        hank_utils:attribute_name(Node) == include
        orelse hank_utils:attribute_name(Node) == include_lib,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

include_lib_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        hank_utils:attribute_name(Node) == include_lib,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

is_unused_local(FilePath, IncludePaths) ->
    not lists:any(fun(IncludePath) -> matches(FilePath, IncludePath) end, IncludePaths).

%% @doc Verifies if FilePath and IncludePath refer both to the same file.
%%      Note that we can't just compare both filename:absname's here, since we
%%      don't really know what is the absolute path of the file referred by
%%      the include directive.
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
    % Note that IncludeLibPaths here are aboslute paths, not relative ones.
    not
        lists:member(
            filename:absname(File), IncludeLibPaths).

expand_lib_dir(IncludeLibPath, Context) ->
    [App | Path] = filename:split(IncludeLibPath),
    case hank_context:app_dir(list_to_atom(App), Context) of
        undefined ->
            IncludeLibPath;
        AppDir ->
            fname_join([AppDir | Path])
    end.

%% @doc Copied verbatim from epp:fname_join(Name).
fname_join(["." | [_ | _] = Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).
