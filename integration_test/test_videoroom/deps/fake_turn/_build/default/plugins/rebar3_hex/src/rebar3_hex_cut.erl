%% @doc `rebar3 hex cut' - Publish with git tag and version bump features
%%
%% Increments tags and publishes.
%%
%% Incrementing and tagging are both optional features of this provider.
%%
%% By default you'll be prompted what type of increment to make to the version or it can be supplied as an argument to
%% the command `-i', `--increment' switch.
%%
%% `cut' will also optionally create version bump commit, create a tag with a name corresponding to the new version, and
%%  prompt you if you'd like to push the tag up to your git repository.
%%
%%
%% Below is a full example of `cut' in action :
%%
%% ```
%% rebar3 hex cut
%% ===> Analyzing applications...
%% ===> Verifying dependencies...
%% Select semver increment or other (Current 0.1.3):
%% 1) patch
%% 2) minor
%% 3) major
%% 4) other
%% [1-4] > 1
%% Create 'v0.1.4' commit? ("Y")>
%% Push master to origin master? ("N")>
%% Local Password:
%% Publishing myapp 0.1.4 to hexpm:myrepo
%% Description: My Application
%% Dependencies:
%%   relx ~>4.5.0
%% Included files:
%%   LICENSE
%%   README.md
%%   rebar.config
%%   src/myapp.app.src
%%   src/myapp.erl
%% Licenses: Apache-2.0
%% Links:
%%   github: https://github.com/myname/myapp
%% Build tools: rebar3
%% Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct
%% Proceed (with warnings)? ("Y")>
%% ===> Published myapp 0.1.4
%% ===> Analyzing applications...
%% ===> Compiling myapp
%% ===> Running edoc for myapp
%% ===> Running ex_doc for myapp
%% ===> Published docs for myapp 0.1.4
%% Create new git tag v0.1.4? ("Y")>
%% ===> Creating new tag v0.1.4...
%% Push new tag to origin? ("Y")>
%% ===> Pushing new tag v0.1.4...
%% '''
%%
%% <ul>
%%  <li>`--repo' - Specify the repository to work with. This option is required when
%%      you have multiple repositories configured, including organizations. The argument must
%%      be a fully qualified repository name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%      Defaults to `hexpm'.
%%   </li>
%%   <li>`-i', `--increment' - Specify the type of version increment to perform without being prompted at runtime.
%%       Supported types are :
%%          <ul>
%%              <li><b>major</b></li>
%%              <li><b>minor</b></li>
%%              <li><b>patch</b></li>
%%          </ul>
%%  </li>
%% </ul>
-module(rebar3_hex_cut).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, cut).
-define(DEPS, [{default, lock}]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex cut"},
                                {short_desc, "Increment version number and publish package"},
                                {desc, ""},
                                {opts, [{increment, $i, "increment", {string, undefined},
                                         "Type of semver increment: major, minor or patch"},
                                        rebar3_hex:repo_opt()]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.


%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar3_hex:task_state(State) of
        {ok, Task} ->
            handle_task(Task);
        {error, Reason} ->
            ?RAISE(Reason)
    end.

%% default path - maybe tag, maybe commit, maybe push commit,  publish, maybe push tag

handle_task(#{args := Args} = Task) ->
    #{repo := Repo, state := State, apps := Apps} = Task,
    Selected = rebar3_hex_io:select_apps(Apps),
    lists:foreach(fun(App) -> cut(State, Repo, App, Args) end, Selected),
    {ok, State}.

%% @private
-spec format_error(any()) -> iolist().
format_error({no_write_key, RepoName}) ->
    io_lib:format("No api key with permissions to write to the repository ~ts was found.", [RepoName]);
format_error({bad_increment, Type}) ->
    io_lib:format("Increment must be major, minor or patch. ~s is not valid.", [Type]);
format_error({invalid_semver, Version}) ->
    Err = "non-semantic version number \"~ts\" found",
    io_lib:format(Err, [Version]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Public API
%% ===================================================================

cut(State, Repo, App, #{} = Args) ->
    {Version, ResolvedVersion} = version_info(State, App),
    MaybeType = maps:get(increment, Args, undefined),
    Type = get_increment(MaybeType, ResolvedVersion),
    NewVersion = increment_version(Type, ResolvedVersion),
    AppSrcFile = rebar_app_info:app_file_src(App),

    case Version of
        _Git when Version =:= git orelse Version =:= "git" ->
            create_tag(NewVersion),

            case try_publish(State, Repo, App, Args) of
                {ok, _State} ->
                    maybe_push_tag(NewVersion);
                _ ->
                    delete_tag(NewVersion),
                   {ok, State}
            end;
        _ ->
            Spec = rebar3_hex_file:update_app_src(App, NewVersion),
            NewAppSrcFile = io_lib:format("~tp.\n", [Spec]),
            ok = rebar_file_utils:write_file_if_contents_differ(AppSrcFile, NewAppSrcFile),
            ask_commit_and_push(NewVersion),
            case try_publish(State, Repo, rebar_app_info:original_vsn(App, NewVersion), Args) of
                {ok, _} ->
                    maybe_create_and_push_tag(NewVersion),
                    {ok, State};
                {error, publish_failed} ->
                    delete_tag(NewVersion),
                    {ok, State}
            end

    end.

maybe_create_and_push_tag(Version) ->
    PromptStr = io_lib:format("Create new git tag v~s?", [Version]),
    case rebar3_hex_io:ask(PromptStr, boolean, "Y") of
        true ->
            create_tag(Version),
            maybe_push_tag(Version);
        false ->
            ok
    end.

create_tag(Version) ->
    rebar_api:info("Creating new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git tag v~s", [Version]), []).

maybe_push_tag(Version) ->
    case rebar3_hex_io:ask("Push new tag to origin?", boolean, "Y") of
        true ->
            push_tag(Version);
        false ->
            ok
    end.

push_tag(Version) ->
    rebar_api:info("Pushing new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git push origin v~s", [Version]), []).

delete_tag(Version) ->
    rebar_api:info("Deleting new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git tag -d v~s", [Version]), []).

try_publish(State, Repo, App, Args) ->
 try rebar3_hex_publish:publish(State, Repo, App, Args) of
    {ok, _} ->
        {ok, State}
    catch
        error:{error, {rebar3_hex_publish, Err}} ->
        ErrStr  = rebar3_hex_publish:format_error(Err),
        rebar_api:error(ErrStr, []),
        {error, publish_failed}
  end.

increment_version(other, _CurrentVersion) ->
     CustomVersion =  rebar3_hex_io:ask("New Version ", string),
     _parsed = parse_version(CustomVersion),
     rebar_utils:to_binary(CustomVersion);

increment_version(Type, VersionStr) when is_list(VersionStr) orelse is_binary(VersionStr) ->
    Parsed = parse_version(VersionStr),
    increment_version(Type, Parsed);

increment_version(Type, Version) ->
    Inc = rebar3_hex_version:increment(Type, Version),
    rebar3_hex_version:format(Inc).

get_increment(undefined, Version) ->
    rebar3_hex_io:say("Select semver increment or other (Current ~s):", [Version]),
    rebar3_hex_io:say("1) patch", []),
    rebar3_hex_io:say("2) minor", []),
    rebar3_hex_io:say("3) major", []),
    rebar3_hex_io:say("4) other", []),
    case rebar3_hex_io:ask("[1-4] ", number) of
        4 ->
            other;
        TypeInt ->
            case int_to_increment(TypeInt) of
                error ->
                    rebar_api:error("Invalid number given, try again~n", []),
                    get_increment(undefined, Version);
                Type ->
                    Type
            end
    end;

get_increment(Type, _) -> normalize_increment(Type).

parse_version(Version) ->
    case rebar3_hex_version:parse(Version) of
        {ok, Parsed} ->
            Parsed;
        _Err ->
         ?RAISE({invalid_semver, Version})
    end.

int_to_increment(1) -> patch;
int_to_increment(2) -> minor;
int_to_increment(3) -> major;
int_to_increment(_) -> error.

version_info(State, App) ->
    Version = rebar_app_info:original_vsn(App),
    Resolved = rebar3_hex_app:vcs_vsn(State, App),
    {Version, Resolved}.

normalize_increment("patch") -> patch;
normalize_increment("minor") -> minor;
normalize_increment("major") -> major;
normalize_increment(_) -> error.

ask_commit_and_push(NewVersion) ->
    case rebar3_hex_io:ask(io_lib:format("Create 'v~s' commit?", [NewVersion]), boolean, "Y") of
        true ->
            rebar_utils:sh(io_lib:format("git commit -a -m 'v~s'", [NewVersion]), []),
            case rebar3_hex_io:ask("Push master to origin master?", boolean, "N") of
                true ->
                    rebar_utils:sh("git push origin master:master", []);
                false ->
                    ok
            end;
        false ->
            ok
    end.
