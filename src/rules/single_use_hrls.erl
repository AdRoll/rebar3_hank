%% @doc A rule to detect header files used in just one module.
%%      <p>To avoid this warning, include the content of the header file into
%%      the module.</p>
-module(single_use_hrls).

-behaviour(hank_rule).

-export([analyze/2]).

%% @private
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [set_result(HeaderFile, IncludedAtFile)
     || {HeaderFile, [IncludedAtFile]} <- build_include_list(FilesAndASTs)].

set_result(HeaderFile, IncludedAtFile) ->
    #{file => HeaderFile,
      line => 0,
      text =>
          iolist_to_binary(io_lib:format("This header file is only included at: ~s",
                                         [IncludedAtFile]))}.

build_include_list(FilesAndASTs) ->
    {Files, _ASTs} = lists:unzip(FilesAndASTs),
    lists:foldl(fun({File, AST}, Acc) ->
                   lists:foldl(fun(IncludedFile, AccInner) ->
                                  AtFiles =
                                      case lists:keyfind(IncludedFile, 1, AccInner) of
                                          false -> [];
                                          {IncludedFile, IncludedAtFiles} -> IncludedAtFiles
                                      end,
                                  NewTuple = {IncludedFile, [File | AtFiles]},
                                  lists:keystore(IncludedFile, 1, AccInner, NewTuple)
                               end,
                               Acc,
                               included_files(Files, AST))
                end,
                [],
                FilesAndASTs).

included_files(Files, AST) ->
    [included_file_path(Files, IncludedFile) || IncludedFile <- include_paths(AST)].

include_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        hank_utils:attribute_name(Node) == include,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].

included_file_path(Files, IncludedFile) ->
    MatchFunc = fun(File) -> matches(IncludedFile, File) end,
    case lists:search(MatchFunc, Files) of
        {value, IncludedFileWithPath} ->
            IncludedFileWithPath;
        false ->
            IncludedFile
    end.

%% @doc Verifies if FilePath and FullIncludePath refer both to the same file.
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
    FilePath == string:find(IncludePath, FilePath, trailing).
