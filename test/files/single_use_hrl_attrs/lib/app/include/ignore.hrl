-hank([{single_use_hrl_attrs,
        ["MACRO_ALL",
         {"MACRO_0", 0},
         {"MACRO_1", 1},
         {"MACRO_NONE", none},
         ignored_record,
         reported_record]}]).

-define(MACRO_ALL, "this macro is always ignored").
-define(MACRO_ALL(), "regardless of its arity").
-define(MACRO_ALL(It), "is never reported as unused").
-define(MACRO_ALL(Not, Even), "if it has multiple arguments").
-define(MACRO_0, "This version of the macro should not be ignored").
-define(MACRO_0(), "This one should since it has 0 arguments").
-define(MACRO_0(And), "this one should be reported since it has one").
-define(MACRO_0(Also, This), "one, that has 2").
-define(MACRO_1, "For this macro").
-define(MACRO_1(), "there should be a report").
-define(MACRO_1(But), "not for this version with arity 1").
-define(MACRO_1(Just, For), "all the others").
-define(MACRO_NONE, "This instance should be ignored").
-define(MACRO_NONE(), "But there should be a report").
-define(MACRO_NONE(For), "each of the").
-define(MACRO_NONE(Other, Versions), "").

-record(ignored_record, {list}).
-record(reported_record, {list}).
