# Changelog

## [0.3.0](https://github.com/AdRoll/rebar3_hank/tree/0.3.0) (2021-03-15)

[Full Changelog](https://github.com/AdRoll/rebar3_hank/compare/0.2.1...0.3.0)

**Implemented enhancements:**

- \[unused\_configuration\_options\] Ignore single config options [\#88](https://github.com/AdRoll/rebar3_hank/issues/88)
- Rename unused\_ignored\_function\_params rule [\#69](https://github.com/AdRoll/rebar3_hank/issues/69)
- Ignore Specific Items [\#68](https://github.com/AdRoll/rebar3_hank/issues/68)
- Parallelize file parsing and rule evaluation [\#29](https://github.com/AdRoll/rebar3_hank/issues/29)
- \[\#68\] Adding support to ignore specific items [\#83](https://github.com/AdRoll/rebar3_hank/pull/83) ([diegomanuel](https://github.com/diegomanuel))

**Fixed bugs:**

- The directive to ignore a check on a module doesn't seem to work [\#87](https://github.com/AdRoll/rebar3_hank/issues/87)
- Display real error when analyze fails [\#86](https://github.com/AdRoll/rebar3_hank/issues/86)
- \[unused callbacks\] A macro with the same name as a callback [\#82](https://github.com/AdRoll/rebar3_hank/issues/82)
- \[unused\_callbacks\] Detect callbacks present in macro definitions [\#81](https://github.com/AdRoll/rebar3_hank/issues/81)
- NIF check makes hank crash in some scenarios [\#79](https://github.com/AdRoll/rebar3_hank/issues/79)
- \[single\_use\_hrl\_files\] Can't ignore a warning [\#67](https://github.com/AdRoll/rebar3_hank/issues/67)
- Ignore unused NIF function parameters [\#65](https://github.com/AdRoll/rebar3_hank/issues/65)
- \[\#65\] Ignoring NIF stubs for `unused\_ignored\_function\_params` rule [\#75](https://github.com/AdRoll/rebar3_hank/pull/75) ([diegomanuel](https://github.com/diegomanuel))
- \[\#67\] Adding support to ignore files that don't exist on filesystem [\#70](https://github.com/AdRoll/rebar3_hank/pull/70) ([diegomanuel](https://github.com/diegomanuel))

**Closed issues:**

- New Rule: Unused configuration options [\#72](https://github.com/AdRoll/rebar3_hank/issues/72)
- New Rule: Unused Callbacks [\#71](https://github.com/AdRoll/rebar3_hank/issues/71)

**Merged pull requests:**

- \#88: Adding docs [\#93](https://github.com/AdRoll/rebar3_hank/pull/93) ([pbrudnick](https://github.com/pbrudnick))
- \#88: Add possibility to ignore config options by config file [\#91](https://github.com/AdRoll/rebar3_hank/pull/91) ([pbrudnick](https://github.com/pbrudnick))
- Fix \#82: Handle macros with the same name as a callback [\#90](https://github.com/AdRoll/rebar3_hank/pull/90) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#81: Handle callback usage in macros [\#89](https://github.com/AdRoll/rebar3_hank/pull/89) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \#72: unused\_configuration\_options rule [\#85](https://github.com/AdRoll/rebar3_hank/pull/85) ([pbrudnick](https://github.com/pbrudnick))
- Fix \#29: Parallelize rule evaluation [\#80](https://github.com/AdRoll/rebar3_hank/pull/80) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#69: Rename unnecessary\_function\_arguments rule [\#78](https://github.com/AdRoll/rebar3_hank/pull/78) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Upgrade formatter to 0.10.1 [\#77](https://github.com/AdRoll/rebar3_hank/pull/77) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#71\] New rule `unused\_callbacks` [\#76](https://github.com/AdRoll/rebar3_hank/pull/76) ([diegomanuel](https://github.com/diegomanuel))
- Change min otp version to 21 [\#74](https://github.com/AdRoll/rebar3_hank/pull/74) ([rjcoelho](https://github.com/rjcoelho))
- \[RTI-8987\] Upgrade plugins [\#73](https://github.com/AdRoll/rebar3_hank/pull/73) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.1](https://github.com/AdRoll/rebar3_hank/tree/0.2.1) (2021-01-18)

[Full Changelog](https://github.com/AdRoll/rebar3_hank/compare/0.2.0...0.2.1)

**Fixed bugs:**

- Macro in record name crashes Hank [\#63](https://github.com/AdRoll/rebar3_hank/issues/63)

**Merged pull requests:**

- Fix \#63: Macros as record names [\#64](https://github.com/AdRoll/rebar3_hank/pull/64) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.0](https://github.com/AdRoll/rebar3_hank/tree/0.2.0) (2021-01-18)

[Full Changelog](https://github.com/AdRoll/rebar3_hank/compare/0.1.2...0.2.0)

**Implemented enhancements:**

- Ignore rules by rule/pattern [\#52](https://github.com/AdRoll/rebar3_hank/issues/52)

**Fixed bugs:**

- Macros can be defined twice, and that breaks Hank [\#60](https://github.com/AdRoll/rebar3_hank/issues/60)
- Using macros as function names crashes hank [\#59](https://github.com/AdRoll/rebar3_hank/issues/59)
- Unicode breaks Hank [\#58](https://github.com/AdRoll/rebar3_hank/issues/58)

**Merged pull requests:**

- Some assorted bug fixes [\#57](https://github.com/AdRoll/rebar3_hank/pull/57) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix formatting of unused params rule [\#62](https://github.com/AdRoll/rebar3_hank/pull/62) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#58\] Support for unicode characters [\#61](https://github.com/AdRoll/rebar3_hank/pull/61) ([diegomanuel](https://github.com/diegomanuel))

## [0.1.2](https://github.com/AdRoll/rebar3_hank/tree/0.1.2) (2021-01-04)

[Full Changelog](https://github.com/AdRoll/rebar3_hank/compare/0.0.1...0.1.2)

**Implemented enhancements:**

- Do some refactor to reduce code duplication and whatnot [\#53](https://github.com/AdRoll/rebar3_hank/issues/53)
- Documentation Link in README [\#43](https://github.com/AdRoll/rebar3_hank/issues/43)
- Implement file ignoring behavior [\#14](https://github.com/AdRoll/rebar3_hank/issues/14)

**Fixed bugs:**

- single\_use\_hrls is not respecting ignored files [\#49](https://github.com/AdRoll/rebar3_hank/issues/49)
- \[\#49\] Respect ignore at `single\_use\_hrls` rule [\#50](https://github.com/AdRoll/rebar3_hank/pull/50) ([diegomanuel](https://github.com/diegomanuel))

**Closed issues:**

- Prepare Slides for HackWeek Demos [\#48](https://github.com/AdRoll/rebar3_hank/issues/48)
- Documentation [\#45](https://github.com/AdRoll/rebar3_hank/issues/45)
- Try Hank on our Systems for HackWeek Demos [\#39](https://github.com/AdRoll/rebar3_hank/issues/39)
- Implement hank\_rule:default\_rules/0 [\#24](https://github.com/AdRoll/rebar3_hank/issues/24)
- Ensure we follow community guidelines [\#15](https://github.com/AdRoll/rebar3_hank/issues/15)
- Detect stuff in hrl files that is used by just one module [\#12](https://github.com/AdRoll/rebar3_hank/issues/12)
- Detect hrl files used in just one module [\#10](https://github.com/AdRoll/rebar3_hank/issues/10)
- Detect hrl files that are unused [\#9](https://github.com/AdRoll/rebar3_hank/issues/9)
- Detect parameters in functions that are ignored in every clause [\#8](https://github.com/AdRoll/rebar3_hank/issues/8)
- Detect unused macros [\#7](https://github.com/AdRoll/rebar3_hank/issues/7)
- Detect unused record fields [\#6](https://github.com/AdRoll/rebar3_hank/issues/6)

**Merged pull requests:**

- \#12: single\_use\_hrl\_attrs rule [\#55](https://github.com/AdRoll/rebar3_hank/pull/55) ([pbrudnick](https://github.com/pbrudnick))
- \[\#53\] Some refactor to reduce code duplication and whatnot [\#54](https://github.com/AdRoll/rebar3_hank/pull/54) ([diegomanuel](https://github.com/diegomanuel))
- \[Fix \#39\] Try hank on our systems for hackweek [\#51](https://github.com/AdRoll/rebar3_hank/pull/51) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#43: Documentation Link [\#47](https://github.com/AdRoll/rebar3_hank/pull/47) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#45: Add documentation [\#46](https://github.com/AdRoll/rebar3_hank/pull/46) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#14: Ignore functionality [\#44](https://github.com/AdRoll/rebar3_hank/pull/44) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#10\] Detect header files used in just one module [\#42](https://github.com/AdRoll/rebar3_hank/pull/42) ([diegomanuel](https://github.com/diegomanuel))
- Fix typo [\#41](https://github.com/AdRoll/rebar3_hank/pull/41) ([pbrudnick](https://github.com/pbrudnick))
- Fix \#7: Find unused macros [\#38](https://github.com/AdRoll/rebar3_hank/pull/38) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#6: Detect unused record fields [\#34](https://github.com/AdRoll/rebar3_hank/pull/34) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#24: Locate the default rules [\#32](https://github.com/AdRoll/rebar3_hank/pull/32) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#9: Add rule to detect unused hrl files [\#30](https://github.com/AdRoll/rebar3_hank/pull/30) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Unused function params rule [\#28](https://github.com/AdRoll/rebar3_hank/pull/28) ([pbrudnick](https://github.com/pbrudnick))
- Fix \#52: Allow ignoring specific rules in rebar.config [\#56](https://github.com/AdRoll/rebar3_hank/pull/56) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.1](https://github.com/AdRoll/rebar3_hank/tree/0.0.1) (2020-12-14)

[Full Changelog](https://github.com/AdRoll/rebar3_hank/compare/57e4ca419dce2d6db87c93649bce065f2e8fdb15...0.0.1)

**Closed issues:**

- Verify that hank is indeed analyzing all files it should be analyzing [\#20](https://github.com/AdRoll/rebar3_hank/issues/20)
- Add Github Actions to verify pull requests [\#18](https://github.com/AdRoll/rebar3_hank/issues/18)
- Format the results returned by hank:analyze/2 in a \_human\_ way [\#17](https://github.com/AdRoll/rebar3_hank/issues/17)
- Traverse all erl / hrl files and get their ASTs [\#4](https://github.com/AdRoll/rebar3_hank/issues/4)
- Build a rebar3 plugin skeleton with just the most basic options [\#3](https://github.com/AdRoll/rebar3_hank/issues/3)
- Add some details to the README.md [\#2](https://github.com/AdRoll/rebar3_hank/issues/2)

**Merged pull requests:**

- Add sample files [\#23](https://github.com/AdRoll/rebar3_hank/pull/23) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \#18: Add github actions [\#21](https://github.com/AdRoll/rebar3_hank/pull/21) ([pbrudnick](https://github.com/pbrudnick))
- Fix \#17: format the results returned by hank [\#27](https://github.com/AdRoll/rebar3_hank/pull/27) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#4: Get all the ASTs and run some rules on them [\#25](https://github.com/AdRoll/rebar3_hank/pull/25) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix \#3: Rebar3 plugin skeleton [\#19](https://github.com/AdRoll/rebar3_hank/pull/19) ([elbrujohalcon](https://github.com/elbrujohalcon))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
