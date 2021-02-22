-module(clean).

-callback used_callback(map()) -> boolean().
-callback used_atom(atom()) -> list().

-export([used_callback/1, used_atom/1]).

used_callback(#{module := Module, state := State}) ->
    Module:used_callback(State).

used_atom(Module) ->
    other_module:call(used_atom, Module).
