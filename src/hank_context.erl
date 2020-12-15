%%% @doc rebar3 contextual data turned into a type that we can also manually build, if needed
-module(hank_context).

-opaque t() :: #{app_dirs := #{atom() => file:filename_all()}}.

-export_type([t/0]).

-export([from_rebar_state/1, empty/0]).

-spec from_rebar_state(rebar_state:t()) -> t().
from_rebar_state(State) ->
    AppDirs =
        maps:from_list([{binary_to_atom(rebar_app_info:name(App), utf8), rebar_app_info:dir(App)}
                        || App <- rebar_state:project_apps(State)]),
    #{app_dirs => AppDirs}.

-spec empty() -> t().
empty() ->
    #{app_dirs => #{}}.
