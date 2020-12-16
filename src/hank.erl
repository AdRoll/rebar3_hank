%%% @doc The Erlang Dead Code Cleaner
-module(hank).

-export([analyze/3]).

%% @doc Runs a list of rules over a list of files and returns all the
%%      dead code pieces it can find.
-spec analyze([file:filename()], [hank_rule:t()], hank_context:t()) ->
                 [hank_rule:result()].
analyze(Files, Rules, Context) ->
    ASTs = [{File, get_ast(File)} || File <- Files],
    [Result || Rule <- Rules, Result <- hank_rule:analyze(Rule, ASTs, Context)].

get_ast(File) ->
    case ktn_dodger:parse_file(File, [{scan_opts, [text]}, no_fail]) of
        {ok, AST} ->
            AST;
        {error, OpenError} ->
            erlang:error({cant_parse, File, OpenError})
    end.
