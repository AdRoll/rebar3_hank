%% @doc A rule to detect header files used in just one module.
-module(single_use_hrls).

-behaviour(hank_rule).

-export([analyze/2]).

-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    [set_result(HeaderFile, IncludedAtFile)
     || {HeaderFile, [IncludedAtFile]} <- build_usage_list(FilesAndASTs)].

set_result(HeaderFile, IncludedAtFile) ->
    #{file => HeaderFile,
      line => 0,
      text =>
          iolist_to_binary(io_lib:format("This header file is only included at: ~s",
                                         [IncludedAtFile]))}.

build_usage_list(FilesAndASTs) ->
    lists:foldl(fun({File, AST}, Acc) ->
                   lists:foldl(fun(IncludePath, AccInner) ->
                                  {_, AtFiles} =
                                      case lists:keytake(IncludePath, 1, AccInner) of
                                          {value, Tuple, _} -> Tuple;
                                          false -> {IncludePath, []}
                                      end,
                                  NewTuple = {IncludePath, [File | AtFiles]},
                                  lists:keystore(IncludePath, 1, AccInner, NewTuple)
                               end,
                               Acc,
                               included_hrls(AST))
                end,
                [],
                FilesAndASTs).

included_hrls(AST) ->
    [HrlFile || HrlFile <- include_paths(AST), filename:extension(HrlFile) == ".hrl"].

include_paths(AST) ->
    [erl_syntax:concrete(IncludedFile)
     || Node <- AST,
        erl_syntax:type(Node) == attribute,
        hank_utils:attribute_name(Node) == include,
        IncludedFile <- erl_syntax:attribute_arguments(Node)].
