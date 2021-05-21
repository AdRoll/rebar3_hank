# rebar3_hank [![Hex pm](http://img.shields.io/hexpm/v/rebar3_hank.svg?style=flat)](https://hex.pm/packages/rebar3_hank)
### The Erlang Dead Code Cleaner

![Kill it with Fire](https://repository-images.githubusercontent.com/321259416/91eb8780-3de1-11eb-83e2-be100515c76b)
> Mr. Scorpio says productivity is up 2%, and it's all because of my motivational techniques, like donuts and the possibility of more donuts to come.
>
> _(Homer Simpson)_

## Build

```bash
$ rebar3 compile
```

## Test

```bash
$ rebar3 test
```

## Usage

Add the plugin to your rebar config:

```erlang
{project_plugins, [rebar3_hank]}
```

Then just call the plugin directly in an existing application:

```bash
$ rebar3 hank # or…
$ rebar3 kiwf # (Kill It With Fire)
```

This will review your project, analyzing every `*.[he]rl` file in it (optionally skipping some folders/files if you want to - see below).
Note that Hank will **not** consider files from your project dependencies for the analysis. It will only check the source code in your current application (_applications_, if you're working in an umbrella project).
It will then apply its rules and produce a list of all the dead code that you can effectively delete and/or refactor.

## Certainty

In principle, Hank should have _Dialyzer_ levels of certainty. That is: if Hank points at some code, you can be **100% sure** it's _dead_.

That means that if you find some false positives (i.e. Hank pointed at some code that was not really dead / should not be deleted), **please** report it as **a bug** and we'll take care of fixing it.

## Configuration

The plugin supports the following configuration options in the `hank` section of `rebar.config`:

* `rules` (`[hank_rule:t()]`):
    - This is the list of rules to apply to the analyzed code. Each rule is a module that should apply the `hank_rule` behavior.
    - If this option is not defined, Hank will apply all [the default rules](src/rules).
* `parsing_style` (`hank:parsing_style()`):
    - This parameter determines if Hank should parse files in a parallel (`rpc:pmap/3`) or sequential (`lists:map/2`) fashion.
    - The default value is `parallel` since it's faster.
    - It's recommended to use `sequential` when reporting bugs since the error descriptions are usually more detailed.
* `ignore` (`[file:filename_all() | {file:filename_all(), hank_rule:t()} | {file:filename_all(), hank_rule:t(), list()}]`):
    - List of wildcard patterns representing the files and rules that Hank will ignore when formatting. Tuple format is used to ignore only a specific rule in those files.
  ```erlang
  {hank,
   [{ignore, [
    "rel/**/*",
    "lib/some_module.erl",
    {"test/**/*.erl", unnecessary_function_arguments}
  ]}]}
  ```
    - For ignoring options for `unused_configuration_options` rule, set a list of keys from the .config files you want to ignore.
  ```erlang
  {hank,
   [{ignore, [
    {"sys.config", unused_configuration_options, [not_used_but_ignored]}
  ]}]}
  ```
    - You can also ignore a specific file adding the attribute `-hank ignore.` to it.
    - And you can ignore specific rules adding the attribute `-hank [hank_rule:t()].` with the list of rules you want to ignore.

### Ignore specific rule items
You can even ignore specific rule items with the `-hank` attribute by giving extra _ignore specifications_ for each rule, example:
```erlang
-hank([single_use_hrls, %% Will ignore the whole rule within the module
       {unused_macros,
          ["ALL", %% Will ignore ?ALL, ?ALL() and ?ALL(X)
           {"ZERO", 0}, %% Will ignore ?ZERO() but not ?ZERO(X) nor ?ZERO
           {"ONE",  1}, %% Will ignore ?ONE(X) but not ?ONE()   nor ?ONE
           {"NONE", none} %% Will ignore ?NONE but not ?NONE(X) nor ?NONE()
           ]},
       {unused_record_fields,
           [a_record, %% Will ignore all fields in #a_record
            {a_record, a_field} %% Will ignore #a_record.a_field
           ]},
       {unused_callbacks,
           [all, %% Will ignore all versions of the all callback (i.e. any arity)
            {just, 1} %% Will ignore just(term()) but not just() nor just(_, _) callbacks
           ]},
       {unused_configuration_options,
           [option, %% Will ignore any appearance of option
            {"this_file.config", option} %% Will ignore option if it appears in "this_file.config"
           ]},
       {single_use_hrl_attrs,
           ["ALL",          %% Will ignore ?ALL, ?ALL() and ?ALL(X)
            {"ZERO", 0},    %% Will ignore ?ZERO() but not ?ZERO(X) nor ?ZERO
            {"ONE",  1},    %% Will ignore ?ONE(X) but not ?ONE()   nor ?ONE
            {"NONE", none}, %% Will ignore ?NONE   but not ?NONE(X) nor ?NONE()
            record_name     %% Will ignore #record_name
           ]},
       {unnecessary_function_arguments,
           [{ignore_me, 2}, %% Will ignore any unused argument from ignore_me/2
            {ignore_me_too, 3, 2}, %% Will ignore the 2nd argument from ignore_me_too/3
            ignore_me_again]}]). %% Will ignore any unused argument from any `ignore_me_again/x` within the module (no matter the arity)
```
Refer to each rule documentation for further details.

## Rules

Find detailed information about the rules provided by Hank in [hex docs](https://hexdocs.pm/rebar3_hank/).

## Sample Output

If Hank detects issues in your code, it will report them as follows…

```
src/lapp.erl:18: maybe_evaluate/3 doesn't need its #2 argument
src/lapp.erl:18: maybe_evaluate/3 doesn't need its #1 argument
src/lapp.erl:15: maybe_evaluate/2 doesn't need its #1 argument
src/lapp.erl:5: ?DEFAULT_SAMPLE_RATE is unused
src/lapp.app.src:0: sample_rate is not used anywhere in the code
```

## Full Example

[**@elbrujohalcon**](https://github.com/elbrujohalcon) presented Hank in a lightning talk at CodeBEAM V SF 2021. Watch his talk where he shows an example of using Hank in an iterative process:

[![Hank @ CodeBEAM V SF 2021](http://img.youtube.com/vi/JWicgBIoUTM/0.jpg)](http://www.youtube.com/watch?v=JWicgBIoUTM "Hank @ CodeBEAM V SF 2021")
