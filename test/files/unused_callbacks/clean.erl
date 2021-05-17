-module(clean).

-callback used_callback(map()) -> boolean().
-callback used_atom(atom()) -> list().
-callback used_in_macro() -> this | one | is | used | through | the | macro.

-define(MACRO_USAGE(M), M:used_in_macro()).

-export([used_callback/1, used_atom/1, used_in_macro/1]).

used_callback(#{module := Module, state := State}) ->
    Module:used_callback(State).

used_atom(Module) ->
    erlang:apply(Module, used_atom, [another_atom]).

used_in_macro(Module) ->
    ?MACRO_USAGE(Module).
