-module ignore.

-include("ignore.hrl").

-export([usage/0]).

usage() ->
    #{ignored =>
          #ignored_record{list =
                              [?MACRO_ALL,
                               ?MACRO_ALL(),
                               ?MACRO_ALL(x),
                               ?MACRO_ALL(x, x),
                               ?MACRO_0(),
                               ?MACRO_1(x),
                               ?MACRO_NONE]},
      reported =>
          #reported_record{list =
                               [?MACRO_0,
                                ?MACRO_0(x),
                                ?MACRO_0(x, x),
                                ?MACRO_1,
                                ?MACRO_1(),
                                ?MACRO_1(x, x),
                                ?MACRO_NONE(),
                                ?MACRO_NONE(x),
                                ?MACRO_NONE(x, x)]}}.
