%% @doc A rule to detect unused configuration options
%%      It will find options that are no longer used around the code:
%%      - All the options from the *.config files
%%         (excepting rebar.config, elvis.config and relx.config)
%%      - The env list inside any *.app.src files
%%      <p>To avoid this warning, remove the unused parameters.</p>
-module(unused_configuration_options).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

-define(IGNORED_FILES, ["rebar.config", "elvis.config", "relx.config"]).

%% @doc It gets the options from .config and .app.src files and then:
%%      1. Builds an index with file/options
%%      2. Gets the atoms used around the .erl and .hrl files
%%      3. Calculates the unused atoms (options) and return the results
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, Context) ->
    % get the config options (keys) by file
    ConfigOptionsByFile =
        [{File, config_options(File, Context)}
         || {File, _AST} <- FilesAndASTs,
            filename:extension(File) == ".config" orelse filename:extension(File) == ".src",
            not is_ignored(File)],

    % get just the options (keys) to search usages
    ConfigOptions = extract_options(ConfigOptionsByFile),

    % get all the options used by the .erl/.hrl files
    Uses =
        [UsedOption
         || {File, AST} <- FilesAndASTs,
            filename:extension(File) == ".erl" orelse filename:extension(File) == ".hrl",
            UsedOption <- options_usage(AST, ConfigOptions)],

    % calculate the unused options
    UnusedOptions = ConfigOptions -- lists:usort(Uses),

    % build resuts
    [result(File, Option)
     || {File, Options} <- ConfigOptionsByFile,
        Option <- Options,
        lists:member(Option, UnusedOptions)].

%% @doc It receives a file path and returns a list of options
%% It's prepared for .config and .app.src files, which contain Erlang Terms
%% If the file cannot be parsed, it will be ignored (like other user's .config files)
-spec config_options(file:filename(), hank_context:t()) -> [atom()].
config_options(File, Context) ->
    case file:consult(File) of
        {ok, [ErlangTerms]} ->
            case is_app_src_file(File) of
                true ->
                    {application, _AppName, Options} = ErlangTerms,
                    EnvOptions = proplists:get_value(env, Options, []),
                    proplists:get_keys(EnvOptions);
                false ->
                    config_keys(ErlangTerms, Context)
            end;
        _ ->
            %% error parsing: ignore the file!
            []
    end.

%% @doc Get all the config keys of the project_apps only.
%% If ConfigTuples is a tuple (a one-tuple config file), it is converted into a proplist.
%% When ConfigTuples files contain more than one tuple, they are parsed as a proplist.
config_keys(ConfigTuples, Context) when is_tuple(ConfigTuples) ->
    config_keys([ConfigTuples], Context);
config_keys(ConfigTuples, Context) ->
    [Key
     || {AppName, Proplist} <- ConfigTuples,
        lists:member(AppName, hank_context:project_apps(Context)),
        Key <- proplists:get_keys(Proplist)].

is_app_src_file(File) ->
    filename:extension(File) == ".src".

extract_options(OptionsByFile) ->
    lists:usort([Option || {_File, FileOptions} <- OptionsByFile, Option <- FileOptions]).

options_usage(_AST, []) ->
    [];
options_usage(AST, Options) ->
    [Option || Node <- AST, Option <- Options, hank_utils:node_has_atom(Node, Option)].

is_ignored(File) ->
    lists:member(
        filename:basename(File), ?IGNORED_FILES).

result(File, Option) ->
    #{file => File,
      line => 0,
      text => hank_utils:format_text("~tw is not used anywhere in the code", [Option]),
      pattern => {File, Option}}.

%% @doc Rule ignore specifications example:
%%      <code>
%%      -hank([{unused_configuration_options,
%%               %% You can use the option name or within a specific file
%%               [%% Will ignore any appearance of option
%%                option,
%%                %% Will ignore option if it appears in "this_file.config"
%%                {"this_file.config", option}
%%               ]}]).
%%      </code>
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored(Option, Option) ->
    true;
ignored({_File, Option}, Option) ->
    true;
ignored(_, _) ->
    false.
