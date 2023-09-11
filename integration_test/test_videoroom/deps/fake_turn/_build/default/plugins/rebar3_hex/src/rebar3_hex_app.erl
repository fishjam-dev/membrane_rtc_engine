%% @private
%%% ===================================================================
%%% App utils
%%% ===================================================================
-module(rebar3_hex_app).

-export([vcs_vsn/2, find/2, get_deps/1, validate/1]).

-define(VALIDATIONS, [
    has_semver,
    has_contributors,
    has_maintainers,
    has_description,
    has_valid_licenses,
    has_unstable_deps
]).

-type app_data() :: #{
    name := string(),
    version := string(),
    details := list(),
    deps := list()
}.

-type validation_errors() :: #{
    warnings => list(),
    errors => list()
}.

-spec find([rebar_app_info:t()], string()) -> {ok, rebar_app_info:t()} | {error, app_not_found}.
find(Apps, Name) ->
    Pred = fun(App) ->
        rebar_app_info:name(App) =:= rebar_utils:to_binary(Name)
    end,
    case lists:filter(Pred, Apps) of
        [App] ->
            {ok, App};
        _ ->
            {error, app_not_found}
    end.

-spec get_deps(list()) -> {ok, list()} | {error, {non_hex_deps, list()}}.
get_deps(Deps) ->
    Top = locks_to_deps(Deps),
    Excluded = [rebar_utils:to_list(N) || {N, {T, _, _, _}, 0} <- Deps, T =/= pkg],
    Excluded1 = [rebar_utils:to_list(N) || {N, {T, _, _}, 0} <- Deps, T =/= pkg],

    case Excluded ++ Excluded1 of
        [] ->
            {ok, Top};
        Forbidden ->
            {error, {non_hex_deps, Forbidden}}
    end.

locks_to_deps(Deps) ->
    lists:foldl(fun(D, Acc) -> lock_to_dep(D, Acc) end, [], Deps).

lock_to_dep({A, {pkg, N, V, _, _}, 0}, Acc) ->
    [{N, [{<<"app">>, A}, {<<"optional">>, false}, {<<"requirement">>, V}]} | Acc];
lock_to_dep({A, {pkg, N, V, _}, 0}, Acc) ->
    [{N, [{<<"app">>, A}, {<<"optional">>, false}, {<<"requirement">>, V}]} | Acc];
lock_to_dep(_, Acc) ->
    Acc.

-spec validate(app_data()) -> ok | {error, validation_errors()}.
validate(AppData) ->
    F = fun(K, Acc) ->
        case validate_app(K, AppData) of
            ok ->
                Acc;
            {warn, Warn} ->
                #{warnings := WarnList} = Acc,
                maps:put(warnings, [Warn | WarnList], Acc);
            {error, Error} ->
                #{errors := ErrList} = Acc,
                maps:put(errors, [Error | ErrList], Acc)
        end
    end,
    case lists:foldl(F, #{warnings => [], errors => []}, ?VALIDATIONS) of
        [] ->
            ok;
        Errors ->
            {error, Errors}
    end.

validate_app(has_unstable_deps, #{deps := Deps}) ->
    case lists:foldl(fun(Dep, Acc) -> is_unstable_dep(Dep, Acc) end, [], Deps) of
        [] ->
            ok;
        PreDeps ->
            {warn, {has_unstable_deps, PreDeps}}
    end;
validate_app(has_semver, #{name := Name, version := Ver}) ->
    case rebar3_hex_version:parse(Ver) of
        {error, invalid_version} ->
            {error, {invalid_semver, {Name, Ver}}};
        _ ->
            ok
    end;
validate_app(has_contributors, #{name := Name, details := AppDetails}) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            {warn, {has_contributors, Name}};
        false ->
            ok
    end;
validate_app(has_maintainers, #{name := Name, details := AppDetails}) ->
    case proplists:is_defined(maintainers, AppDetails) of
        true ->
            {warn, {has_maintainers, Name}};
        false ->
            ok
    end;
validate_app(has_description, #{name := Name, details := AppDetails}) ->
    case is_empty_prop(description, AppDetails) of
        true ->
            {warn, {no_description, Name}};
        false ->
            ok
    end;
validate_app(has_valid_licenses, #{name := Name, details := AppDetails}) ->
    case is_empty_prop(licenses, AppDetails) of
        true ->
            {warn, {no_license, Name}};
        _ ->
            Licenses = proplists:get_value(licenses, AppDetails),
            case lists:filter(fun(L) -> not hex_licenses:valid(rebar_utils:to_binary(L)) end,  Licenses) of
                [] -> 
                    ok;
                [_|_] = Invalids -> 
                    {warn, {invalid_licenses, Invalids, Name}}
            end
    end.

is_empty_prop(K, PropList) ->
    Prop = proplists:get_value(K, PropList),
    case Prop of
        Empty when Empty =:= [] orelse Empty =:= undefined ->
            true;
        _ ->
            false
    end.

is_unstable_dep({_, {pkg, Pkg, Ver, _, _}, _}, Acc) ->
    case rebar3_hex_version:parse(Ver) of
        {ok, #{pre := Pre}} when Pre =/= [] ->
            [{Pkg, Ver} | Acc];
        _ ->
            Acc
    end;
is_unstable_dep(_, Acc) ->
    Acc.

-spec vcs_vsn(rebar_state:t(), rebar_app_info:t()) -> string().
vcs_vsn(State, App) ->
    Version = rebar_app_info:original_vsn(App),
    AppDir = rebar_app_info:dir(App),
    Resources = rebar_state:resources(State),
    vcs_vsn(Version, AppDir, Resources).

vcs_vsn(OriginalVsn, Dir, Resources) when is_list(Dir), is_list(Resources) ->
    FakeState = rebar_state:new(),
    case rebar_app_info:new(fake, OriginalVsn, Dir) of
        {ok, AppInfo} ->
            vcs_vsn(
                AppInfo,
                OriginalVsn,
                rebar_state:set_resources(FakeState, Resources)
            );
        Error ->
            Error
    end;
vcs_vsn(AppInfo, Vcs, State) ->
    case vcs_vsn_cmd(AppInfo, Vcs, State) of
        {plain, VsnString} ->
            VsnString;
        {cmd, CmdString} ->
            cmd_vsn_invoke(CmdString, rebar_app_info:dir(AppInfo));
        unknown ->
            rebar_utils:abort("vcs_vsn: Unknown vsn format: ~p", [Vcs]);
        {error, Reason} ->
            rebar_utils:abort("vcs_vsn: ~ts", [Reason])
    end.

%% Temp work around for repos like relx that use "semver"
vcs_vsn_cmd(_, Vsn, _) when is_binary(Vsn) ->
    {plain, Vsn};
vcs_vsn_cmd(AppInfo, VCS, State) when VCS =:= semver; VCS =:= "semver" ->
    vcs_vsn_cmd(AppInfo, git, State);
vcs_vsn_cmd(_AppInfo, {cmd, _Cmd} = Custom, _) ->
    Custom;
vcs_vsn_cmd(AppInfo, {file, File}, _) ->
    Path = filename:join(rebar_app_info:dir(AppInfo), File),
    case file:read_file(Path) of
        {ok, Vsn} ->
            {plain, rebar_utils:to_list(rebar_string:trim(Vsn))};
        Error ->
            Error
    end;
vcs_vsn_cmd(AppInfo, VCS, State) when is_atom(VCS) ->
    rebar_resource_v2:make_vsn(AppInfo, VCS, State);
vcs_vsn_cmd(AppInfo, {VCS, _} = V, State) when is_atom(VCS) ->
    rebar_resource_v2:make_vsn(AppInfo, V, State);
vcs_vsn_cmd(AppInfo, VCS, State) when is_list(VCS) ->
    try list_to_existing_atom(VCS) of
        AVCS ->
            case vcs_vsn_cmd(AppInfo, AVCS, State) of
                unknown -> {plain, VCS};
                Other -> Other
            end
    catch
        error:badarg ->
            {plain, VCS}
    end;
vcs_vsn_cmd(_, _, _) ->
    unknown.

cmd_vsn_invoke(Cmd, Dir) ->
    case rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]) of
        {ok, VsnString} ->
            rebar_string:trim(VsnString, trailing, "\n");
        Error ->
            Error
    end.
