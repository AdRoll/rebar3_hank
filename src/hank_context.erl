%%% @doc rebar3 contextual data turned into a type that we can also manually build, if needed
-module(hank_context).

-opaque t() :: #{app_dirs := #{atom() => file:filename()}}.

-export_type([t/0]).

-export([from_rebar_state/1, new/1]).
-export([app_dir/2]).

%% @doc Build a context from a rebar3 state
-spec from_rebar_state(rebar_state:t()) -> t().
from_rebar_state(State) ->
    new(maps:from_list([{binary_to_atom(rebar_app_info:name(App), utf8),
                         rebar_app_info:dir(App)}
                        || App <- rebar_state:project_apps(State)])).

%% @doc Build a context from scratch
-spec new(#{atom() => file:filename()}) -> t().
new(AppDirs) ->
    #{app_dirs => AppDirs}.

%% @doc Return the root folder for an app
-spec app_dir(atom(), t()) -> undefined | file:filename().
app_dir(App, #{app_dirs := AppDirs}) ->
    maps:get(App, AppDirs, undefined).
