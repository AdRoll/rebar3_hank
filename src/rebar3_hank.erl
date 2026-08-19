-module(rebar3_hank).
-moduledoc """
Main entry point for the rebar3 hank plugin.
""".

-export([init/1]).

-doc false.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_hank_prv:init(State).
