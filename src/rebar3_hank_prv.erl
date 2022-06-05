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
      "Warn on unused ignores (default: true)."},
      {output_json_file,
        $o,
        "output_json_file",
        string,
        "Output Json File Name (default: empty string)"}
    ].

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
            write_data_to_json_file(Results, State),
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

-spec write_data_to_json_file([hank_rule:result()], rebar_state:t()) -> ok.
write_data_to_json_file(Result, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case lists:keyfind(output_json_file, 1, Args) of
        {output_json_file, JsonFilePath} ->
            case valid_json_format(JsonFilePath) of
                true ->
                    ConvertedResult = convert_data_to_binary(Result),
                    EncodedResult = jsx:encode(ConvertedResult),
                    ok = file:write_file(JsonFilePath, EncodedResult);
                false ->
                    ok
            end;
        _ ->
            ok
    end.

-spec valid_json_format(string()) -> boolean().
valid_json_format(JsonFilePath) ->
    JsonFileName = lists:last(string:tokens(JsonFilePath, "/")),
    case lists:last(string:tokens(JsonFileName, ".")) of
        "json" ->
            true;
        _ ->
            false
    end.

-spec convert_data_to_binary([hank_rule:result()]) -> list().
convert_data_to_binary(Data) ->
    Func =
        fun(RuleDetailMap) ->
            #{file := FileName, line := Line, rule := RuleBroken, text := Description} = RuleDetailMap,
            #{
                <<"path">> => to_binary(FileName),
                <<"start_line">> => Line,
                <<"hank_rule_broken">> => to_binary(RuleBroken),
                <<"title">> => compute_title(RuleBroken),
                <<"message">> => to_binary(Description)}
        end,
    [Func(RuleDetails) || RuleDetails <- Data].

-spec compute_title(atom()) -> binary().
compute_title(RuleBroken) ->
    case RuleBroken of
        unused_macros ->
            <<"Unused Macros">>;
        single_use_hrl_attrs ->
            <<"Macro is only used once">>;
        unused_record_fields ->
            <<"Field in the record is unused">>;
        unused_hrls ->
            <<"Unused hrl files">>;
        unused_configuration_options ->
            <<"Unused config">>;
        unused_callbacks ->
            <<"Unused callback functions">>;
        unnecessary_function_arguments ->
            <<"Unused function arguments found">>;
        single_use_hrls ->
            <<"Hrl is only used once">>
    end.

to_binary(Input) when is_atom(Input) ->
    atom_to_binary(Input, utf8);
to_binary(Input) when is_integer(Input) ->
    integer_to_binary(Input);
to_binary(Input) when is_float(Input) ->
    float_to_binary(Input, [{decimals, 10}, compact]);
to_binary(Input) when is_list(Input) ->
    list_to_binary(Input);
to_binary(Input) when is_pid(Input) ->
    list_to_binary(pid_to_list(Input));
to_binary(Input) -> Input.

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
