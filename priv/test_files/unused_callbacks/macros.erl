-module(macros).

-callback used_callback() -> this | one | is | used | through | the | macro.
-callback unused_callback() -> this | one | isnt | used.

-define(unused_callback, {this, macro, doesnt, use, the, callback}).
-define(CALLBACK, used_callback).

-export([using_the_callback/1, not_using_the_callback/0]).

using_the_callback(Module) ->
    Module:?CALLBACK().

not_using_the_callback() ->
    ?unused_callback.
