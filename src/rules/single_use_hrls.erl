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
          hank_utils:format_text("This header file is only included at: ~ts", [IncludedAtFile])}.

build_include_list(FilesAndASTs) ->
    {Files, _ASTs} = lists:unzip(FilesAndASTs),
    lists:foldl(fun({File, AST}, Acc) ->
                   lists:foldl(fun(IncludedFile, AccInner) ->
                                  AtFiles =
                                      case lists:keyfind(IncludedFile, 1, AccInner) of
                                          false ->
                                              [];
                                          {IncludedFile, IncludedAtFiles} ->
                                              IncludedAtFiles
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
    [included_file_path(Files, IncludedFile)
     || IncludedFile <- hank_utils:attr_args_concrete(AST, include),
        is_file_included(Files, IncludedFile) =/= false].

included_file_path(Files, IncludedFile) ->
    case is_file_included(Files, IncludedFile) of
        false ->
            IncludedFile;
        IncludedFileWithPath ->
            IncludedFileWithPath
    end.

is_file_included(Files, IncludedFile) ->
    MatchFunc = fun(File) -> hank_utils:paths_match(IncludedFile, File) end,
    case lists:search(MatchFunc, Files) of
        {value, IncludedFileWithPath} ->
            IncludedFileWithPath;
        _ ->
            false
    end.
