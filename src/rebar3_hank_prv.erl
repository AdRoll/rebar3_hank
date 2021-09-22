%%% @doc Plugin provider for rebar3 hank
-module(rebar3_hank_prv).

-export([init/1, do/1, format_error/1]).

-define(FILES_PATTERN, "**/*.{erl,hrl,config,app.src,app.src.script}").

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    HankProvider =
        providers:create([{name, hank},
                          {module, rebar3_hank_prv},
                          {bare, true},
                          {deps, [app_discovery]},
                          {example, "rebar3 hank"},
                          {opts, opts()},
                          {short_desc, "A rebar plugin for dead code cleaning"},
                          {desc, ""}]),
    KiwFProvider =
        providers:create([{name, kiwf},
                          {module, rebar3_hank_prv},
                          {bare, true},
                          {deps, [app_discovery]},
                          {example, "rebar3 kiwf"},
                          {opts, opts()},
                          {short_desc, "An alias for rebar3 hank"},
                          {desc, ""}]),
    {ok,
     rebar_state:add_provider(
         rebar_state:add_provider(State, HankProvider), KiwFProvider)}.

opts() ->
    [{unused_ignores,
      $u,
      "unused_ignores",
      boolean,
      "Warn on unused ignores (default: true)."}].

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, iodata()}.
do(State) ->
    rebar_api:info("Looking for code to kill with fire...", []),
    Rules = get_rules(State),
    rebar_api:debug("Hank rules: ~p", [Rules]),
    Context = hank_context:from_rebar_state(State),
    rebar_api:debug("Hank Context: ~p", [Context]),
    %% All files except those under _build or _checkouts
    Files = [F || F <- filelib:wildcard(?FILES_PATTERN), not is_hidden(F)],
    rebar_api:debug("Hank will use ~p files for analysis: ~p", [length(Files), Files]),
    IgnoreSpecsFromState =
        case proplists:get_value(ignore, rebar_state:get(State, hank, []), none) of
            none ->
                [];
            IgnoreRules ->
                [{F, Rule, Options}
                 || {Wildcard, Rule, Options} <- normalize(IgnoreRules),
                    F <- filelib:wildcard(Wildcard)]
        end,
    ParsingStyle =
        proplists:get_value(parsing_style, rebar_state:get(State, hank, []), parallel),
    try hank:analyze(Files, IgnoreSpecsFromState, Rules, ParsingStyle, Context) of
        #{results := [],
          unused_ignores := UnusedIgnores,
          stats := Stats} ->
            instrument(Stats, UnusedIgnores, State),
            {ok, State};
        #{results := Results,
          unused_ignores := UnusedIgnores,
          stats := Stats} ->
            instrument(Stats, UnusedIgnores, State),
            {error, format_results(Results)}
    catch
        Kind:Error:Stack ->
            rebar_api:warn("~p analyzing files: ~p\nStack: ~p", [Kind, Error, Stack]),
            {error, format_error(Error)}
    end.

instrument(#{ignored := Ignored,
             parsing := Parsing,
             analyzing := Analyzing,
             total := Total},
           UnusedIgnores,
           State) ->
    rebar_api:debug("Hank ignored ~p warnings", [Ignored]),
    rebar_api:debug("Hank spent ~pms parsing and ~pms analyzing the system (~pms total time)",
                    [Parsing, Analyzing, Total]),
    {Args, _} = rebar_state:command_parsed_args(State),
    Verbose =
        case lists:keyfind(unused_ignores, 1, Args) of
            {unused_ignores, Value} ->
                Value;
            false ->
                true % The default is to print out warnings
        end,
    case {Verbose, UnusedIgnores} of
        {false, _} ->
            ok;
        {true, []} ->
            ok;
        {true, UnusedIgnores} ->
            Msg = "The following ignore specs are no longer needed and can be removed:\n"
                  ++ lists:flatmap(fun format_unused_ignore/1, UnusedIgnores),
            rebar_api:warn(Msg, [])
    end.

format_unused_ignore({File, Rule, all}) ->
    io_lib:format("* ~ts: ~p~n", [File, Rule]);
format_unused_ignore({File, Rule, Specs}) ->
    io_lib:format("* ~ts: ~p: ~p~n", [File, Rule, Specs]).

-spec format_results([hank_rule:result()]) -> string().
format_results(Results) ->
    lists:foldr(fun(Result, Acc) -> [Acc, format_result(Result), $\n] end,
                "The following pieces of code are dead and should be removed:\n",
                Results).

format_result(#{file := File,
                line := Line,
                text := Msg}) ->
    hank_utils:format_text("~ts:~tp: ~ts", [File, Line, Msg]).

%% @private
%% @doc Determines files that should be fully hidden to Hank.
is_hidden(Filename) ->
    lists:any(fun is_hidden_name/1, filename:split(Filename)).

is_hidden_name(".") ->
    false;
is_hidden_name("..") ->
    false;
is_hidden_name("." ++ _) ->
    true;
is_hidden_name("_" ++ _) ->
    true;
is_hidden_name(_) ->
    false.

%% @private
-spec format_error(any()) -> binary().
format_error(Reason) ->
    hank_utils:format_text("~tp", [Reason]).

-spec get_rules(rebar_state:t()) -> [hank_rule:t()].
get_rules(State) ->
    case proplists:get_value(rules, rebar_state:get(State, hank, []), all) of
        all ->
            hank_rule:default_rules();
        Rules ->
            Rules
    end.

normalize(IgnoreRules) ->
    lists:foldl(fun (WildcardRuleMaybeOpts, Acc) when is_tuple(WildcardRuleMaybeOpts) ->
                        normalize_rules(WildcardRuleMaybeOpts, Acc);
                    (Wildcard, Acc) ->
                        [{Wildcard, all, all} | Acc]
                end,
                [],
                IgnoreRules).

normalize_rules({Wildcard, Rules, Options}, Acc) when is_list(Rules) ->
    [{Wildcard, Rule, Options} || Rule <- Rules] ++ Acc;
normalize_rules({Wildcard, Rule, Options}, Acc) ->
    normalize_rules({Wildcard, [Rule], Options}, Acc);
normalize_rules({Wildcard, Rules}, Acc) when is_list(Rules) ->
    [{Wildcard, Rule, all} || Rule <- Rules] ++ Acc;
normalize_rules({Wildcard, Rule}, Acc) ->
    normalize_rules({Wildcard, [Rule]}, Acc).
