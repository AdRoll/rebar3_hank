%% @doc A rule to detect unused header files.
%%      <p>To avoid this warning, remove the unused header files.</p>
%%
%%      <h3>Note</h3>
%%      <blockquote>
%%      This rule assumes that hrl files will not be used outside your project.
%%      If you are writing a library that requires your clients to use some of
%%      your header files, you can add an ignore rule in rebar.config for it.
%%      </blockquote>
%% @todo Figure out the absname of IncludePath
%%       [https://github.com/AdRoll/rebar3_hank/issues/31]
-module(unused_hrls).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

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
       text => "This file is unused",
       pattern => undefined}
     || File <- Files,
        filename:extension(File) == ".hrl",
        is_unused_local(File, IncludePaths),
        is_unused_lib(File, IncludeLibPaths)].

include_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        % Yeah, include_lib can also be used as include ¯\_(ツ)_/¯ (check epp's code)
        hank_utils:node_has_attrs(Node, [include, include_lib]),
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

include_lib_paths(AST) ->
    hank_utils:attr_args_concrete(AST, include_lib).

is_unused_local(FilePath, IncludePaths) ->
    not
        lists:any(fun(IncludePath) -> hank_utils:paths_match(IncludePath, FilePath) end,
                  IncludePaths).

is_unused_lib(File, IncludeLibPaths) ->
    % Note that IncludeLibPaths here are absolute paths, not relative ones.
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

%% @doc It doesn't make sense to provide individual ignore spec support here.
%%      The rule's basic unit is already a file.
-spec ignored(hank_rule:ignore_pattern(), term()) -> false.
ignored(undefined, _IgnoreSpec) ->
    false.
