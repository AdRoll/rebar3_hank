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
    Rules = get_rules(State),
    rebar_api:debug("Hank rules: ~p", [Rules]),
    Files = filelib:wildcard("**/*.[he]rl"),
    rebar_api:debug("Analyzing ~p files: ~p", [length(Files), Files]),
    try hank:analyze(Files, Rules) of
        [] ->
            {ok, State};
        Results ->
            {error, format_results(Results)}
    catch
        Kind:Error:Stack ->
            rebar_api:warning("~p analyzing files: ~p\nStack: ~p", [Kind, Error, Stack]),
            {error, format_error(Error)}
    end.

%% @private
%% @todo properly format the warnings [https://github.com/AdRoll/rebar3_hank/issues/17]
-spec format_results([hank_rule:result()]) -> string().
format_results(Results) ->
    lists:foldr(fun(Result, Acc) -> [Acc, io_lib:format("~p~n", [Result])] end,
                "The following pieces of code are dead and should be removed:\n",
                Results).

%% @private
-spec format_error(any()) -> string().
format_error(Reason) ->
    io_lib:format("Unknown Formatting Error: ~p", [Reason]).

-spec get_rules(rebar_state:t()) -> all | [hank_rule:t()].
get_rules(State) ->
    proplists:get_value(rules, rebar_state:get(State, hank, []), all).
