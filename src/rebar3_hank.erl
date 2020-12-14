%%% @doc Main entry point for the rebar3 hank plugin
-module(rebar3_hank).

-export([init/1]).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_hank_prv:init(State).
