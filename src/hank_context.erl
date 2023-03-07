%%% @doc rebar3 contextual data turned into a type that we can also manually build, if needed
-module(hank_context).

-type apps_dirs() :: #{atom() => file:filename()}.

-opaque t() :: #{app_dirs := apps_dirs(), project_apps := [atom()]}.

-export_type([t/0, apps_dirs/0]).

-export([from_rebar_state/1, new/2]).
-export([app_dir/2, project_apps/1]).

%% @doc Build a context from a rebar3 state.
-spec from_rebar_state(rebar_state:t()) -> t().
from_rebar_state(State) ->
    AppDirs =
        [{binary_to_atom(rebar_app_info:name(App), utf8), rebar_app_info:dir(App)}
         || App <- rebar_state:project_apps(State)],
    ProjectApps = proplists:get_keys(AppDirs),
    new(maps:from_list(AppDirs), ProjectApps).

%% @doc Build a context from scratch.
-spec new(apps_dirs(), [atom()]) -> t().
new(AppDirs, ProjectApps) ->
    #{app_dirs => AppDirs, project_apps => ProjectApps}.

%% @doc Return the root folder for an app.
-spec app_dir(atom(), t()) -> undefined | file:filename().
app_dir(App, #{app_dirs := AppDirs}) ->
    maps:get(App, AppDirs, undefined).

%% @doc OTP applications included in the project.
-spec project_apps(t()) -> [atom()].
project_apps(#{project_apps := ProjectApps}) ->
    ProjectApps.
