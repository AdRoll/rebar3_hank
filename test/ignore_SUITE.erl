%%% @doc Test module for the ignore functionality
-module(ignore_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([rebar_config/1, hank_ignore/1, hank_individual_rules/1]).

all() ->
    [rebar_config, hank_ignore, hank_individual_rules].

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
