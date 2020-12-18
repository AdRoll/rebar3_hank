-module(gen_server_imp).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, my_function/1, my_other_function/2]).

init(_) ->
    ignore.

handle_call(_M, _From, State) ->
    {noreply, State}.

handle_cast(_M, State) ->
    {noreply, State}.

% ignoring param
my_function(_) ->
    ok.

my_other_function(undefined, _B) ->
    undefined;
my_other_function(A, _B) ->
    A.
