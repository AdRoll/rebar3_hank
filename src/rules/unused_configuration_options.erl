%% @doc A rule to detect unused configuration options
%%      It will find options that are no longer used around the code:
%%       - All the options from the `*.config` files
%%         (excepting rebar.config, elvis.config and relx.config)
%%       - The `env` list inside any `*.app.src` files
-module(unused_configuration_options).

-behaviour(hank_rule).

-export([analyze/2, ignored/2]).

-define(IGNORED_FILES, ["rebar.config", "elvis.config", "relx.config"]).

%% @doc It gets the options from .config and .app.src files and then:
%%      1. Builds an index with file/options
%%      2. Gets the atoms used around the .erl and .hrl files
%%      3. Calculates the unused atoms (options) and return the results
-spec analyze(hank_rule:asts(), hank_context:t()) -> [hank_rule:result()].
analyze(FilesAndASTs, _Context) ->
    % get the config options (keys) by file
    ConfigOptionsByFile =
        [{File, config_options(File)}
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

%% It receives a file path and returns a list of options
%% It's prepared for .config and .app.src files, which contain Erlang Terms
%% If the file is a .app.src one, it only retrieves the keys under the `env` proplist.
-spec config_options(file:filename()) -> [atom()].
config_options(File) ->
    {ok, [ErlangTerms]} = file:consult(File),
    case is_app_src_file(File) of
        true ->
            {application, _AppName, Options} = ErlangTerms,
            EnvOptions = proplists:get_value(env, Options, []),
            proplists:get_keys(EnvOptions);
        false ->
            config_keys(ErlangTerms)
    end.

config_keys(ConfigTuples) ->
    [Key || {_AppName, Proplist} <- ConfigTuples, Key <- proplists:get_keys(Proplist)].

is_app_src_file(File) ->
    filename:extension(File) == ".src".

extract_options(OptionsByFile) ->
    [Option || {_File, FileOptions} <- OptionsByFile, Option <- FileOptions].

options_usage(_AST, []) ->
    [];
options_usage(AST, Options) ->
    Functions = [Node || Node <- AST, erl_syntax:type(Node) == function],
    [Option || Option <- Options, is_option_used(Option, Functions)].

is_option_used(Option, Functions) ->
    lists:any(fun(Function) -> hank_utils:function_has_atom(Function, Option) end, Functions).

is_ignored(File) ->
    lists:member(
        filename:basename(File), ?IGNORED_FILES).

result(File, Option) ->
    #{file => File,
      line => 0,
      text => hank_utils:format_text("~tw is not used anywhere in the code", [Option])}.

%% @todo Add ignore pattern support
-spec ignored(hank_rule:ignore_pattern(), term()) -> boolean().
ignored(undefined, _IgnoreSpec) ->
    false; %% Remove this clause and just use the one below
ignored(_Pattern, _IgnoreSpec) ->
    true.