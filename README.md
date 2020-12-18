# rebar3_hank [![Hex pm](http://img.shields.io/hexpm/v/rebar3_format.svg?style=flat)](https://hex.pm/packages/rebar3_hank)
### The Erlang Dead Code Cleaner

![Kill it with Fire](https://repository-images.githubusercontent.com/321259416/91eb8780-3de1-11eb-83e2-be100515c76b)
> Mr. Scorpio says productivity is up 2%, and it's all because of my motivational techniques, like donuts and the possibility of more donuts to come.
>
> _(Homer Simpsons)_

## Build

```bash
$ rebar3 compile
```

## Test

```bash
$ rebar3 test
```

## Use

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
It will then apply its rules and produce a list of all the dead code that you can effectively delete and/or refactor.

## Certainty

In principle, Hank should have _Dialyzer_ levels of certainty. That is: if Hank points at some code, you can be **100% sure** it's _dead_.

That means that if you find some false positives (i.e. Hank pointed at some code that was not really dead / should not be deleted), **please** report it as **a bug** and we'll take care of fixing it.

## Configuration

The plugin supports the following configuration options in the `hank` section of `rebar.config`:

* `rules` (`[hank_rule:t()]`):
    - This is the list of rules to apply to the analyzed code. Each rule is a module that should apply the `hank_rule` behavior.
    - If this option is not defined, Hank will apply all [the default rules](src/rules).
* `ignore` (`[file:filename_all()]`):
    - List of wildcard patterns representing the files that Hank will ignore when formatting.
    - You can also ignore a specific file adding the attribute `-hank ignore.` to it.
    - And you can ignore specific rules adding the attribute `-hank [hank_rule:t()].` with the list of rules you want to ignore.

## Rules

Find detailed information about the rules provided by Hank in [hex docs](https://hexdocs.pm/rebar3_hank/).
