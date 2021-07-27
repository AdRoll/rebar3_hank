%%% @doc Test module for the ignore functionality
-module(ignore_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([rebar_config/1, hank_ignore/1, hank_individual_rules/1, rebar_config_ignore/1]).

-elvis([{elvis_style, dont_repeat_yourself, disable}]). % for rebar_config_ignore/1

all() ->
    [rebar_config, hank_ignore, hank_individual_rules, rebar_config_ignore].

init_per_testcase(_, Config) ->
    hank_test_utils:init_per_testcase(Config, "ignore").

end_per_testcase(_, Config) ->
    hank_test_utils:end_per_testcase(Config).

%% @doc No warning should be emitted for files listed in the ignored part of
%%      hank's config at rebar.config
rebar_config(_Config) ->
    State = hank_test_utils:init(),

    ct:comment("Warnings should be emitted since we're not ignoring the problematic "
               "files"),
    State1 = rebar_state:set(State, hank, []),
    Warnings = find_warnings(State1),

    ct:comment("If we ignore the problematic files, we should not get warnings "
               "for them"),
    State2 =
        rebar_state:set(State,
                        hank,
                        [{ignore, [binary_to_list(File) || #{file := File} <- Warnings]}]),
    {ok, _} = rebar3_hank_prv:do(State2),

    ct:comment("If we ignore some rules on the problematic files, we should "
               "not get warnings for them"),
    State3 =
        rebar_state:set(State,
                        hank,
                        [{ignore,
                          [{binary_to_list(File), global_rejector}
                           || #{file := File, text := <<" global_rejector">>} <- Warnings]}]),
    Warnings3 = find_warnings(State3),
    [] = [x || #{text := <<" global_rejector">>} <- Warnings3],
    true = length(Warnings3) > 0,
    [] = Warnings3 -- Warnings,
    true = length(Warnings -- Warnings3) > 0,

    {comment, ""}.

%% @doc No warning should be emitted for files with -hank ignore
hank_ignore(_Config) ->
    %% Initialize rebar3 state as if we run `rebar3 hank` with the default rules
    State = hank_test_utils:init(),

    ct:comment("With -hank ignore, there should be no warnings"),
    Warnings = find_warnings(State),
    [] =
        [Warning || Warning = #{file := File} <- Warnings, string:equal(File, "with_ignore.erl")],
    [] =
        [Warning || Warning = #{file := File} <- Warnings, string:equal(File, "with_ignore.hrl")],

    {comment, ""}.

%% @doc No warning should be emitted for rules ignored with -hank [rule, ...]
hank_individual_rules(_Config) ->
    State = hank_test_utils:init(),

    ct:comment("With -hank ignore, there should only be warnings for non-ignored "
               "rules"),
    Rules = [unused_macros, unnecessary_function_arguments, global_rejector],
    State1 = rebar_state:set(State, hank, [{rules, Rules}]),
    Warnings = find_warnings(State1),
    [<<" global_rejector">>] =
        [Text
         || #{file := File, text := Text} <- Warnings, string:equal(File, "specific_ignore.erl")],

    {comment, ""}.

%% @doc No warning should be emmited for lists of ignored rules and neither should
%%      evaluation of the code fail
rebar_config_ignore(_Config) ->
    Rule1 = unused_macros,
    Rule2 = unnecessary_function_arguments,
    FileErl = "rebar_config_ignore.erl",
    Rules = [Rule1, Rule2],

    State0 = hank_test_utils:init(),
    State1 = rebar_state:set(State0, hank, []),

    ct:comment("Prepare for the next test (we start with whatever worked)"),
    State2 =
        rebar_state:set(State1,
                        hank,
                        [{rules, Rules}, {ignore, [{FileErl, Rule1}, {FileErl, Rule2}]}]),
    Warnings0 = find_warnings(State2),
    [] = [Warning0 || Warning0 = #{file := File} <- Warnings0, string:equal(File, FileErl)],

    ct:comment("Test with a list of _ignore_"),
    State3 =
        rebar_state:set(State2, hank, [{rules, Rules}, {ignore, [{FileErl, [Rule1, Rule2]}]}]),
    Warnings1 = find_warnings(State3),
    [] = [Warning1 || Warning1 = #{file := File} <- Warnings1, string:equal(File, FileErl)],

    {comment, ""}.

find_warnings(State) ->
    {error, Error} = rebar3_hank_prv:do(State),
    <<"The following pieces of code",
      " are dead and should be removed:\n",
      ResultsBin/binary>> =
        iolist_to_binary(Error),
    Results = binary:split(ResultsBin, <<$\n>>, [global, trim]),

    lists:map(fun(Result) ->
                 [File, Line | Text] = binary:split(Result, <<$:>>, [global, trim]),
                 #{file => File,
                   line => Line,
                   text => iolist_to_binary(Text)}
              end,
              Results).
