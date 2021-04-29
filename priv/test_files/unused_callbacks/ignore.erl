-module(ignore).

-hank([{unused_callbacks, [all_arities, {just_one, 1}]}]).

-callback all_arities() -> ignored.
-callback all_arities(_) -> ignored.
-callback all_arities(_, _) -> ignored.
-callback just_one() -> reported.
-callback just_one(_) -> ignored.
-callback just_one(_, _) -> reported.
