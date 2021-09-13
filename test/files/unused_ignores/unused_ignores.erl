-module(unused_ignores).

-hank([{unnecessary_function_arguments, [{non_exported_function_with, 2, 1}]}]).
-hank([{unused_macros, ["MACRO", "MICRO"]}]).
-hank([bad_rule]).
