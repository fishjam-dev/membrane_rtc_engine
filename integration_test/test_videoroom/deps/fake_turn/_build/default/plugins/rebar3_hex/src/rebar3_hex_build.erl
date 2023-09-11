% @doc `rebar3 hex build' - Build packages and docs
%
%% Builds a new local version of your package.
%%
%% By default this provider will build both a package tarball and docs tarball.
%%
%% The package and docs .tar files are created in the current directory, but is not pushed to the repository. An app
%% named foo at version 1.2.3 will be built as foo-1.2.3.tar. Likewise the docs .tar would be built as
%% foo-1.2.4-docs.tar.
%%
%% ```
%% $ rebar3 hex build
%% '''
%%
%% You may also build only a package or docs tarball utilizing the same available command line options.
%%
%% ```
%% $ rebar3 hex build package
%% '''
%%
%% ```
%% $ rebar3 hex build docs
%% '''
%%
%% <h2>Configuration</h2>
%% Packages are configured via `src/<myapp>.app.src'  attributes.
%%
%% == Required configuration ==
%%
%% <ul>
%%  <li>
%%      `application' - application name. This is required per Erlang/OTP thus it should always be present anyway.
%%  </li>
%   <li>
%       `vsn' -  must be a valid <a href="https://semver.org/">semantic version</a> identifier.
%   </li>
%%  <li>
%%      `licenses' - A list of licenses the project is licensed under. This attribute is required and must be a valid
%%      <a href="https://spdx.org/licenses/">spdx</a> identifier.
%%  </li>
%% </ul>
%%
%%
%% == Optional configuration ==
%% In addition, the following meta attributes are supported and highly recommended :
%%
%% <ul>
%%  <li>
%%      `description' - a brief description about your application.
%%  </li>
%%
%%  <li>
%%      `pkg_name' - The name of the package in case you want to publish the package with a different name than the
%%       application name.
%%  </li>
%%
%%  <li>
%%      `links' - A map where the key is a link name and the value is the link URL. Optional but highly
%%      recommended.
%%  </li>
%%
%%  <li> `files' - A list of files and directories to include in the package. Defaults to standard project directories,
%%        so you usually don't need to set this property.
%%  </li>
%%  <li>
%%      `include_paths' - A list of paths containing files you wish to include in a release.
%%  </li>
%%  <li>
%%      `exclude_paths' - A list of paths containing files you wish to exclude in a release.
%%  </li>
%%  <li>
%%      `exclude_patterns' - A list of regular expressions used to exclude files that may have been accumulated via
%%      `files' and `include_paths' and standard project paths.
%%  </li>
%%  <li>
%%      `build_tools' - List of build tools that can build the package. It's very rare that you need to set this.
%%  </li>
%% </ul>
%%
%% Below is an example :
%%
%% ```
%% {application, myapp,
%%  [{description, "An Erlang/OTP application"},
%%   {vsn, "0.1.0"},
%%   {modules, []},
%%   {registered, []},
%%   {applications, [kernel,
%%                   stdlib,
%%                   ]},
%%   {licenses, ["Apache-2.0"]},
%%   {links, [{"GitHub", "https://github.com/my_name/myapp"}]}]}.
%% '''
%%
%% <h2> Command line options </h2>
%%
%% <ul>
%%  <li> `-r', `--repo' - Specify the repository to work with. This option is required when
%%      you have multiple repositories configured, including organizations. The argument must
%%      be a fully qualified repository name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%      Defaults to `hexpm'.
%%   </li>
%%   <li> `-u', `--unpack' - Builds the tarball and unpacks contents into a directory. Useful for making sure the tarball
%%        contains all needed files before publishing. See --output below for setting the output path.
%%   </li>
%%   <li> `-o', `--output' - Sets output path. When used with --unpack it means the directory
%%   (Default: `<app>-<version>'). Otherwise, it specifies tarball path (Default: `<app>-<version>.tar').
%%   Artifacts will be written to `_build/<profile>/lib/<your_app>/' by default.
%%   </li>
%% </ul>

-module(rebar3_hex_build).

-export([create_package/2, create_docs/3, create_docs/4]).

-include("rebar3_hex.hrl").

-define(DEFAULT_FILES, [
    "src",
    "c_src",
    "include",
    "rebar.config.script",
    "priv",
    "rebar.config",
    "rebar.lock",
    "CHANGELOG*",
    "changelog*",
    "README*",
    "readme*",
    "LICENSE*",
    "license*",
    "NOTICE"
]).

-define(DEPS, [{default, compile}, {default, lock}]).
-define(PROVIDER, build).
-define(DEFAULT_DOC_DIR, "doc").

-export([
    init/1,
    do/1,
    format_error/1
]).

%% Helpers
-export([doc_opts/2]).

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
        {example, "rebar3 hex build"},
        {short_desc, "Builds a new local version of your package and docs."},
        {desc, ""},
        {opts, [
            rebar3_hex:repo_opt(),
            {app, $a, "app", {string, undefined}, "Specify the app to build."},
            {output_dir, $o, "output", {string, undefined}, "Specify the directory to output artifacts to."},
            {unpack, $u, "unpack", {boolean, false}, "Unpack the contents of tarballs generated vs writing them out to the filesystem."}
        ]}
    ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Task = rebar3_hex:task_state(State, get_repo(State)),
    handle_task(Task).

get_repo(State) ->
     {Args, _} = rebar_state:command_parsed_args(State),
     case proplists:get_value(repo, Args, undefined) of
         undefined ->
             rebar3_hex_config:default_repo(State);
         RepoName ->
            rebar3_hex_config:repo(State, RepoName)
     end.

%% @private
-spec format_error(any()) -> iolist().
format_error({build_package, Error}) when is_list(Error) ->
    io_lib:format("Error building package : ~ts", [Error]);

format_error({build_package, {error, Error}}) when is_list(Error) ->
    io_lib:format("Error building package : ~ts", [Error]);

format_error({build_docs, {error, no_doc_config}}) ->
    no_doc_config_messsage();

format_error({build_docs, {error, {doc_provider_not_found, PrvName}}}) ->
    doc_provider_not_found(PrvName);

format_error({build_docs, {error, missing_doc_index}}) ->
    doc_missing_index_message();

format_error({build_docs, Error}) when is_list(Error) ->
    io_lib:format("Error building docs : ~ts", [Error]);

format_error(repo_required_for_docs) ->
   Str =  "Error :~n\tA repo argument is required when building docs if multiple repos exist"
    " and at least one has doc configuration.~n\tSpecify a repo argument or run"
    " rebar3 hex build package if you only need to build a package.",
    io_lib:format(Str, []);

format_error(app_switch_required) ->
     "--app switch is required when building packages or docs in a umbrella with multiple apps";

format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

no_doc_config_messsage() ->
    "No doc provider has been specified in your hex config.\n"
    "Be sure to add a doc provider to the hex config you rebar configuration file.\n\n"
    "Example : {hex, [{doc, ex_doc}]\n".

doc_missing_index_message() ->
    "An index.html file was not found in docs after running docs provider.\n"
    "Be sure the docs provider is configured correctly and double check it by running it on its own\n".

doc_provider_not_found(Provider) ->
    io_lib:format("The doc provider ~ts specified in your hex config could not be found", [Provider]).

handle_task(#{apps := [_,_|_]}) ->
    ?RAISE(app_switch_required);

handle_task(#{state := State, repo := Repo, apps := [App], args := #{task := docs} = Args}) ->
    case create_docs(State, Repo, App) of
        {ok, Docs} ->
            AbsDir = write_or_unpack(App, Docs, Args),
            rebar3_hex_io:say("Your docs can be inspected at ~ts", [AbsDir]),
            {ok, State};
        Error ->
            ?RAISE({build_docs, Error})
    end;

handle_task(#{state := State, apps := [App], args := #{task := package} = Args}) ->
    case create_package(State, App) of
        {ok, Pkg} ->
            AbsDir = write_or_unpack(App, Pkg, Args),
            rebar3_hex_io:say("Your package contents can be inspected at ~ts", [AbsDir]),
            {ok, State};
        Error ->
            ?RAISE({build_package, Error})
    end;

handle_task(#{state := State, repo := Repo, apps := [App], args := Args}) ->
    case create_package(State, App) of
        {ok, Pkg} ->
            AbsOutput = write_or_unpack(App, Pkg, Args),
            rebar3_hex_io:say("Your package tarball is available at ~ts", [AbsOutput]),
            case create_docs(State, Repo, App) of
                {ok, Docs} ->
                    AbsFile = write_or_unpack(App, Docs, Args),
                    rebar3_hex_io:say("Your docs tarball is available at ~ts", [AbsFile]),
                    {ok, State};
                {error, no_doc_config} ->
                    rebar_api:warn(no_doc_config_messsage(), []),
                    {ok, State};
                {error, {doc_provider_not_found, PrvName}} ->
                    rebar_api:warn(doc_provider_not_found(PrvName), []),
                    {ok, State};
                {error, missing_doc_index} ->
                    rebar_api:warn(doc_missing_index_message(), []),
                    {ok, State};
                Error ->
                    ?RAISE({build_docs, Error})
            end;
        Error ->
            ?RAISE({build_package, Error})
    end.

output_path(docs, Name, Version, #{unpack := true}) ->
    io_lib:format("~ts-~ts-docs", [Name, Version]);
output_path(docs, Name, Version, _Args) ->
    io_lib:format("~ts-~ts-docs.tar", [Name, Version]);
output_path(package, Name, Version, #{unpack := true}) ->
    io_lib:format("~ts-~ts", [Name, Version]);
output_path(package, Name, Version, _Args) ->
    io_lib:format("~ts-~ts.tar", [Name, Version]).

write_or_unpack(App, #{type := Type, tarball := Tarball, name := Name, version := Version}, Args) ->
    OutputDir = output_dir(App, Args),
    Out = output_path(Type, Name, Version, Args),
    AbsOut = filename:join(OutputDir, Out),
    case Args of
        #{unpack := true} ->
            file:make_dir(AbsOut),
            case Type of
                docs ->
                    hex_tarball:unpack_docs(Tarball, AbsOut);
                package ->
                    hex_tarball:unpack(Tarball, AbsOut)
            end;
        _ ->
            file:write_file(AbsOut, Tarball)
    end,
    AbsOut.

%% We are exploiting a feature of ensuredir that that creates all
%% directories up to the last element in the filename, then ignores
%% that last element. This way we ensure that the dir is created
%% and not have any worries about path names
output_dir(App, #{output_dir := undefined}) ->
    Dir = filename:join([rebar_app_info:out_dir(App), "hex"]),
    filelib:ensure_dir(filename:join(Dir, "tmp")),
    Dir;
output_dir(_App, #{output_dir := Output}) ->
    Dir = filename:join(filename:absname(Output), "tmp"),
    filelib:ensure_dir(Dir),
    Dir;
output_dir(App, _) ->
    Dir = filename:join([rebar_app_info:out_dir(App), "hex"]),
    filelib:ensure_dir(filename:join(Dir, "tmp")),
    Dir.

%% @private
create_package(State, App) ->
    Name = rebar_app_info:name(App),
    Version = rebar3_hex_app:vcs_vsn(State, App),
    {application, _, AppDetails} = rebar3_hex_file:update_app_src(App, Version),

    LockDeps = rebar_state:get(State, {locks, default}, []),
    case rebar3_hex_app:get_deps(LockDeps) of
        {ok, TopLevel} ->
            AppDir = rebar_app_info:dir(App),
            Config = rebar_config:consult(AppDir),
            ConfigDeps = proplists:get_value(deps, Config, []),
            Deps1 = update_versions(ConfigDeps, TopLevel),
            Description = proplists:get_value(description, AppDetails, ""),
            PackageFiles = include_files(Name, AppDir, AppDetails),
            Licenses = proplists:get_value(licenses, AppDetails, []),
            Links = proplists:get_value(links, AppDetails, []),
            BuildTools = proplists:get_value(build_tools, AppDetails, [<<"rebar3">>]),

            %% We check the app file for the 'pkg' key which allows us to select
            %% a package name other then the app name, if it is not set we default
            %% back to the app name.
            PkgName = binarify(proplists:get_value(pkg_name, AppDetails, Name)),

            Optional = [
                {<<"app">>, Name},
                {<<"parameters">>, []},
                {<<"description">>, binarify(Description)},
                {<<"files">>, [binarify(File) || {File, _} <- PackageFiles]},
                {<<"licenses">>, binarify(Licenses)},
                {<<"links">>, to_map(binarify(Links))},
                {<<"build_tools">>, binarify(BuildTools)}
            ],
            OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
            Metadata = maps:from_list([
                {<<"name">>, PkgName},
                {<<"version">>, binarify(Version)},
                {<<"requirements">>, maps:from_list(Deps1)}
                | OptionalFiltered
            ]),

            case create_package_tarball(Metadata, PackageFiles) of
                {error, _} = Err ->
                    Err;
                Tarball ->
                    Package = #{
                        type => package,
                        name => PkgName,
                        deps => Deps1,
                        version => Version,
                        metadata => Metadata,
                        files => PackageFiles,
                        tarball => Tarball,
                        has_checkouts => has_checkouts(State)
                    },
                    {ok, Package}
            end;
        Error ->
            Error
    end.

update_versions(ConfigDeps, LockDeps) ->
    [
        begin
            case lists:keyfind(binary_to_atom(N, utf8), 1, ConfigDeps) of
                {_, V} when is_binary(V) ->
                    Req = {<<"requirement">>, V},
                    {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))};
                {_, V} when is_list(V) ->
                    Req = {<<"requirement">>, binarify(V)},
                    {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))};
                _ ->
                    %% using version from lock. prepend ~> to make it looser
                    {_, Version} = lists:keyfind(<<"requirement">>, 1, M),
                    Req = {<<"requirement">>, <<"~>", Version/binary>>},
                    {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))}
            end
        end
     || {N, M} <- LockDeps
    ].

include_files(Name, AppDir, AppDetails) ->
    AppSrc = {application, to_atom(Name), AppDetails},
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    %% In versions prior to v7 the name of the for including paths and excluding paths was include_files and
    %% exclude_files. We don't document this anymore, but we do support it to avoid breaking changes. However,
    %% users should be instructed to use *_paths. Likewise for exclude_regexps which is now documented as
    %% exclude_patterns.
    IncludePaths = proplists:get_value(include_paths, AppDetails, proplists:get_value(include_files, AppDetails, [])),
    ExcludePaths = proplists:get_value(exclude_paths, AppDetails, proplists:get_value(exclude_files, AppDetails, [])),
    ExcludeRes = proplists:get_value(exclude_patterns, AppDetails, proplists:get_value(exclude_regexps, AppDetails, [])),

    AllFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(FilePaths, AppDir)),
    IncludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(IncludePaths, AppDir)),
    ExcludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(ExcludePaths, AppDir)),

    %% We filter first and then include, that way glob excludes can be
    %% overwritten be explict includes
    FilterExcluded = lists:filter(
        fun({_, Path}) ->
            not exclude_file(Path, ExcludeFiles, ExcludeRes)
        end,
        AllFiles
    ),
    WithIncludes = lists:ukeymerge(2, FilterExcluded, IncludeFiles),

    AppFileSrc = filename:join("src", rebar_utils:to_list(Name) ++ ".app.src"),
    AppSrcBinary = binarify(lists:flatten(io_lib:format("~tp.\n", [AppSrc]))),
    lists:keystore(AppFileSrc, 1, WithIncludes, {AppFileSrc, AppSrcBinary}).

exclude_file(Path, ExcludeFiles, ExcludeRe) ->
    lists:keymember(Path, 2, ExcludeFiles) orelse
        known_exclude_file(Path, ExcludeRe).

known_exclude_file(Path, ExcludeRe) ->
    KnownExcludes = [
        %% emacs temp files
        "~$",
        %% c object files
        "\\.o$",
        %% compiled nif libraries
        "\\.so$",
        %% vim swap files
        "\\.swp$"
    ],
    lists:foldl(
        fun
            (_, true) -> true;
            (RE, false) -> re:run(Path, RE) =/= nomatch
        end,
        false,
        KnownExcludes ++ ExcludeRe
    ).

%% Note that we return a list
has_checkouts(State) ->
    filelib:is_dir(rebar_dir:checkouts_dir(State)).

%% @private
create_docs(State, Repo, App) ->
    create_docs(State, Repo, App, #{doc_dir => undefined}).

%% @private
-dialyzer({nowarn_function, create_docs/4}).
create_docs(State, Repo, App, Args) ->
    case maybe_gen_docs(State, Repo, App, Args) of
        {ok, DocDir} ->
            case docs_detected(DocDir) of
                true ->
                    AppDetails = rebar_app_info:app_details(App),
                    Files = rebar3_hex_file:expand_paths([DocDir], DocDir),
                    Name = rebar_utils:to_list(rebar_app_info:name(App)),
                    PkgName = rebar_utils:to_list(proplists:get_value(pkg_name, AppDetails, Name)),
                    OriginalVsn = rebar_app_info:original_vsn(App),
                    Vsn = rebar_utils:vcs_vsn(App, OriginalVsn, State),
                    case create_docs_tarball(Files) of
                        {ok, Tarball} ->
                            {ok, #{
                                type => docs, tarball => Tarball, name => binarify(PkgName), version => binarify(Vsn)
                            }};
                        {error, Reason} ->
                            {error, hex_tarball:format_error(Reason)};
                        Err ->
                            Err
                    end;
                false ->
                    {error, missing_doc_index}
            end;
        {error, _} = Err ->
            Err;
        Err ->
            {error, Err}
    end.

maybe_gen_docs(_State, _Repo, App, #{doc_dir := DocDir}) when is_list(DocDir) ->
    AppDir = rebar_app_info:dir(App),
    {ok, filename:absname(filename:join(AppDir, DocDir))};
maybe_gen_docs(State, Repo, App, _Args) ->
    case doc_opts(State, Repo) of
        {ok, PrvName} ->
            case providers:get_provider(PrvName, rebar_state:providers(State)) of
                not_found ->
                    {error, {doc_provider_not_found, PrvName}};
                Prv ->
                    case providers:do(Prv, State) of
                        {ok, _State1} ->
                            {ok, resolve_dir(App, PrvName)};
                        _ ->
                            {error, {doc_provider_failed, PrvName}}
                    end
            end;
        _ ->
            {error, no_doc_config}
    end.

resolve_dir(App, PrvName) ->
    AppDir = rebar_app_info:dir(App),
    AppOpts = rebar_app_info:opts(App),
    DocOpts =
        case PrvName of
            edoc ->
                rebar_opts:get(AppOpts, edoc_opts, []);
            _ ->
                rebar_opts:get(AppOpts, PrvName, [])
        end,
    DocDir = proplists:get_value(dir, DocOpts, ?DEFAULT_DOC_DIR),
    filename:absname(filename:join(AppDir, DocDir)).

docs_detected(DocDir) ->
    filelib:is_file(DocDir ++ "/index.html").

-spec doc_opts(rebar_state:t(), map()) -> {ok, atom()} | undefined.
doc_opts(State, Repo) ->
    case Repo of
        #{doc := #{provider := PrvName}} when is_atom(PrvName) ->
            Deprecation = "Setting doc options in repo configuration has been deprecated."
                          " You should configure a docmentation provider in top level hex "
                          " configuration now.",
            rebar_api:warn(Deprecation, []),
            {ok, PrvName};
        _ ->
            Opts = rebar_state:opts(State),
            case proplists:get_value(doc, rebar_opts:get(Opts, hex, []), undefined) of
                undefined -> undefined;
                PrvName when is_atom(PrvName) -> {ok, PrvName};
                #{provider := PrvName} -> {ok, PrvName};
                _ -> undefined
            end
    end.

binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Map) when is_map(Map) ->
    maps:from_list(binarify(maps:to_list(Map)));
binarify(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true ->
            rebar_utils:to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

-dialyzer({nowarn_function, create_package_tarball/2}).
create_package_tarball(Metadata, Files) ->
    case hex_tarball:create(Metadata, Files) of
        {ok, #{tarball := Tarball, inner_checksum := _Checksum}} ->
            Tarball;
        {error, Reason} ->
            {error, hex_tarball:format_error(Reason)};
        Error ->
            Error
    end.

-dialyzer({nowarn_function, create_docs_tarball/1}).
create_docs_tarball(Files) ->
    case hex_tarball:create_docs(Files) of
        {ok, Tarball} ->
            {ok, Tarball};
        Error ->
            Error
    end.

-spec to_atom(atom() | string() | binary() | integer() | float()) ->
    atom().
to_atom(X) when erlang:is_atom(X) ->
    X;
to_atom(X) when erlang:is_list(X) ->
    list_to_existing_atom(X);
to_atom(X) ->
    to_atom(rebar_utils:to_list(X)).

to_map(Map) when is_map(Map) ->
    Map;
to_map(List) when is_list(List) ->
    maps:from_list(List).
