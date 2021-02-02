%%% @doc Plugin provider for rebar3 hank
-module(rebar3_hank_prv).

-export([init/1, do/1, format_error/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    HankProvider =
        providers:create([{name, hank},
                          {module, rebar3_hank_prv},
                          {bare, true},
                          {deps, [app_discovery]},
                          {example, "rebar3 hank"},
                          {opts, []},
                          {short_desc, "A rebar plugin for dead code cleaning"},
                          {desc, ""}]),
    KiwFProvider =
        providers:create([{name, kiwf},
                          {module, rebar3_hank_prv},
                          {bare, true},
                          {deps, [app_discovery]},
                          {example, "rebar3 kiwf"},
                          {opts, []},
                          {short_desc, "An alias for rebar3 hank"},
                          {desc, ""}]),
    {ok,
     rebar_state:add_provider(
         rebar_state:add_provider(State, HankProvider), KiwFProvider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, iodata()}.
do(State) ->
    rebar_api:info("Looking for code to kill with fire...", []),
    Rules = get_rules(State),
    rebar_api:debug("Hank rules: ~p", [Rules]),
    Context = hank_context:from_rebar_state(State),
    rebar_api:debug("Hank Context: ~p", [Context]),
    %% All files except those under _build or _checkouts
    Files = [F || F <- filelib:wildcard("**/*.[he]rl"), hd(F) /= $_],
    rebar_api:debug("Hank will use ~p files for anlysis: ~p", [length(Files), Files]),
    IgnoredFiles =
        case proplists:get_value(ignore, rebar_state:get(State, hank, []), none) of
            none ->
                [];
            IgnoreRules ->
                [{F, Rule}
                 || {Wildcard, Rule} <- normalize(IgnoreRules), F <- ignore_wildcard(Wildcard)]
        end,
    try hank:analyze(Files, IgnoredFiles, Rules, Context) of
        #{results := [], ignored := 0} ->
            {ok, State};
        #{results := [], ignored := Ignored} ->
            rebar_api:debug("Hank ignored ~p warnings", [Ignored]),
            {ok, State};
        #{results := Results, ignored := 0} ->
            {error, format_results(Results)};
        #{results := Results, ignored := Ignored} ->
            rebar_api:debug("Hank ignored ~p warnings", [Ignored]),
            {error, format_results(Results)}
    catch
        Kind:Error:Stack ->
            rebar_api:warn("~p analyzing files: ~p\nStack: ~p", [Kind, Error, Stack]),
            {error, format_error(Error)}
    end.

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
    lists:map(fun ({Wildcard, Rule}) ->
                      {Wildcard, Rule};
                  (Wildcard) ->
                      {Wildcard, all}
              end,
              IgnoreRules).

%% @doc Allows to ignore files that don't exist on filesystem
ignore_wildcard(Wildcard) ->
    case filelib:wildcard(Wildcard) of
        [] ->
            [Wildcard];
        Files ->
            Files
    end.
