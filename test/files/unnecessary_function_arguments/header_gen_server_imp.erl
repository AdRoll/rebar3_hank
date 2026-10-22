-module(header_gen_server_imp).
-moduledoc """
This module implements an OTP behaviour declared in an included header file.
Only the behaviour callbacks are exempt.
""".

-include("gen_server_in_header.hrl").

-export([init/1, handle_call/3, handle_cast/2, my_function/1]).

init(_) ->
    ignore.

handle_call(_M, _From, State) ->
    {noreply, State}.

handle_cast(_M, State) ->
    {noreply, State}.

my_function(_) ->
    this_will_warn.
