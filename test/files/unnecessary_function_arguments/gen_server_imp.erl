-module(gen_server_imp).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, my_function/1, my_other_function/2]).

init(_) ->
    ignore.

handle_call(_M, _From, State) ->
    {noreply, State}.

handle_cast(_M, State) ->
    {noreply, State}.

my_function(_) ->
    this_will_warn.

my_other_function(this_also_will_warn, _B) ->
    undefined;
my_other_function(A, _B) ->
    A.
