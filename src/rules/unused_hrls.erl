-module(unused_hrls).
-moduledoc """
A rule to detect unused header files.

To avoid this warning, remove the unused header files.

> ## Note
> This rule assumes that hrl files will not be used outside your project.
> If you are writing a library that requires your clients to use some of
> your header files, you can add an ignore rule in `rebar.config` for it.
> If the path of an include is not literal text (e.g. a macro), Hank can't
> tell which header file it refers to, so no header file is marked as unused.
""".
-moduledoc #{
    todo =>
        "[Figure out the absname of IncludePath](https://github.com/AdRoll/rebar3_hank/issues/31)."
}.

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

-doc false.
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, Context) ->
    case
        lists:any(
            fun({_File, AST}) ->
                hank_utils:has_non_literal_attr_args(AST, [include, include_lib])
            end,
            FilesAndASTs
        )
    of
        true ->
            % Such an include could refer to any header file
            [];
        false ->
            analyze_literal_includes(FilesAndASTs, Context)
    end.

analyze_literal_includes(FilesAndASTs, Context) ->
    {Files, ASTs} = lists:unzip(FilesAndASTs),
    IncludePaths = [IncludePath || AST <- ASTs, IncludePath <- include_paths(AST)],
    IncludeLibPaths =
        [
            expand_lib_dir(IncludeLibPath, Context)
         || AST <- ASTs, IncludeLibPath <- include_lib_paths(AST)
        ],
    [
        #{
            file => File,
            line => 0,
            text => "This file is unused",
            pattern => undefined
        }
     || File <- Files,
        filename:extension(File) =:= ".hrl",
        is_unused_local(File, IncludePaths),
        is_unused_lib(File, IncludeLibPaths)
    ].

include_paths(AST) ->
    [
        erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        % Yeah, include_lib can also be used as include ¯\_(ツ)_/¯ (check epp's code)
        hank_utils:node_has_attrs(Node, [include, include_lib]),
        % It's none for an attribute without arguments, like -include.
        erl_syntax:attribute_arguments(Node) =/= none,
        IncludedFile <- erl_syntax:attribute_arguments(Node)
    ].

include_lib_paths(AST) ->
    hank_utils:attr_args_concrete(AST, include_lib).

is_unused_local(FilePath, IncludePaths) ->
    not lists:any(
        fun(IncludePath) -> hank_utils:paths_match(IncludePath, FilePath) end,
        IncludePaths
    ).

is_unused_lib(File, IncludeLibPaths) ->
    % Note that IncludeLibPaths here are absolute paths, not relative ones.
    not lists:member(
        filename:absname(File), IncludeLibPaths
    ).

expand_lib_dir(IncludeLibPath, Context) ->
    case filename:split(unicode:characters_to_list(IncludeLibPath)) of
        [App | Path] ->
            case app_dir(App, Context) of
                undefined ->
                    IncludeLibPath;
                AppDir ->
                    fname_join([AppDir | Path])
            end;
        [] ->
            IncludeLibPath
    end.

-doc """
The project's applications are existing atoms, so a name that is not one (which may not
even fit in an atom) is not one of them.
""".
app_dir(App, Context) ->
    try list_to_existing_atom(App) of
        AppName ->
            hank_context:app_dir(AppName, Context)
    catch
        error:badarg ->
            undefined
    end.

-doc """
Copied verbatim from `epp:fname_join(Name).`
""".
fname_join(["." | [_ | _] = Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).

-doc """
It doesn't make sense to provide individual ignore spec support here.
The rule's basic unit is already a file.
""".
-spec ignored(hank_rule:ignore_pattern(), term()) -> false.
ignored(undefined, _IgnoreSpec) ->
    false.
