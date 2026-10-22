-module(own_and_header_behaviour_imp).
-moduledoc """
This module implements an OTP behaviour declared in the module itself and
another one declared in an included header file.
The callbacks of both of them are exempt.
""".

-behaviour(application).

-include("gen_server_in_header.hrl").

-export([start/2, stop/1, init/1, handle_call/3, handle_cast/2, my_function/1]).

start(_Type, _Args) ->
    ok.

stop(_State) ->
    ok.

init(_) ->
    ignore.

handle_call(_M, _From, State) ->
    {noreply, State}.

handle_cast(_M, State) ->
    {noreply, State}.

my_function(_) ->
    this_will_warn.
