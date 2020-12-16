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
    rebar_api:debug("Hank will analyze ~p files: ~p", [length(Files), Files]),
    try hank:analyze(Files, Rules, Context) of
        [] ->
            {ok, State};
        Results ->
            {error, format_results(Results)}
    catch
        Kind:Error:Stack ->
            rebar_api:warn("~p analyzing files: ~p\nStack: ~p", [Kind, Error, Stack]),
            {error, format_error(Error)}
    end.

%% @private
%% @todo properly format the warnings [https://github.com/AdRoll/rebar3_hank/issues/17]
-spec format_results([hank_rule:result()]) -> string().
format_results(Results) ->
    lists:foldr(fun(Result, Acc) -> [Acc, format_result(Result), $\n] end,
                "The following pieces of code are dead and should be removed:\n",
                Results).

format_result(#{file := File,
                line := Line,
                text := Msg}) ->
    io_lib:format("~s:~p: ~s", [File, Line, Msg]).

%% @private
-spec format_error(any()) -> string().
format_error(Reason) ->
    io_lib:format("Unknown Formatting Error: ~p", [Reason]).

-spec get_rules(rebar_state:t()) -> [hank_rule:t()].
get_rules(State) ->
    case proplists:get_value(rules, rebar_state:get(State, hank, []), all) of
        all ->
            hank_rule:default_rules();
        Rules ->
            Rules
    end.
