-module(warnings).

-callback unused_callback(map()) -> boolean().
-callback used_callback(map()) -> boolean().

-export([unused_callback/1, used_callback/1, call_callback/1]).

unused_callback(#{module := Module, state := State}) ->
    other_module:call(Module, State).

used_callback(State) ->
    other_module:do(State).

call_callback(#{state := State}) ->
    used_callback(State).
