-module(warnings).

-callback unused_callback(map()) -> boolean().
-callback used_callback(map()) -> boolean().

-export([unused_callback/1, call_callback/1]).

%% This function doesn't count as callback usage because
%% it's just a regular module function that happens to have
%% the same name as the callback definition, but it may have
%% nothing to do with the callback itself like in this example
%% in where `other_module:call/2` is "opaque" to us and its
%% implementation may not use this module callbacks at all
unused_callback(#{module := Module, state := State}) ->
    other_module:call(Module, State).

%% This function counts as callback usage (although it really isn't)
%% because it's being called from `call_callback/1` and Hank detects
%% the atom `used_callback` in it, although `other_module:do/1`
%% implementation may not use this module callbacks at all
used_callback(State) ->
    other_module:do(State).

call_callback(#{state := State}) ->
    used_callback(State).
