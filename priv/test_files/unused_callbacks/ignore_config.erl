-module(ignore_config).

-callback all_arities() -> ignored.
-callback all_arities(_) -> ignored.
-callback all_arities(_, _) -> ignored.
-callback just_one() -> reported.
-callback just_one(_) -> ignored.
-callback just_one(_, _) -> reported.
