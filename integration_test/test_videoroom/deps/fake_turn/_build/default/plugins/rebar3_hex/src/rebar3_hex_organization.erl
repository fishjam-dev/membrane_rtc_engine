%% @doc
%% `rebar3_hex_organization' - Manage organizations
%%
%% Manages the list of authorized hex organizations.
%% 
%% Note that all commands that require a `NAME' argument expect a qualified repository name for the
%% argument (i.e., `hexpm:my_org').
%%
%% == About Organizations == 
%%
%% Organizations are feature provided by hexpm that allows you group packages, public and private alike. Organizations 
%%  are treated as repositories that have a parent. The parent is found as the first part of a repository's name, 
%%  separated from the organization by a `:'. So for the organization `your_org' on the main repository `hexpm' 
%%  the fully qualified name would be `hexpm:your_org'.
%%
%% Be sure to add your organization to either your global rebar.config `~/.config/rebar3/rebar.config' or
%% within an projects `rebar.config'. Below is an example: 
%%
%% ```
%% {hex, [{repos, [ #{name => <<"hexpm:your_org">>}]}]}.
%% '''
%%
%% == Authorize an organization ==
%% 
%% This command will generate an API key used to authenticate access to the organization. See the `rebar3_hex_user' 
%% tasks to list and control all your active API keys.
%%
%% ```
%% $ rebar3 hex organization auth NAME  [--key KEY] [--key-name KEY_NAME]
%% '''
%%
%% == Deauthorize and remove an organization ==
%%
%% ```
%% $ rebar3 hex organization deauth NAME
%% '''
%%
%% == List all authorized organizations  ==
%%
%% This command will only list organizations you have authorized with this task, it will not list organizations you
%% have access to by having authorized with `rebar3 hex user auth'.
%%
%% == Generate organization key ==
%% This command is useful to pre-generate keys for use with `rebar3 hex organization auth NAME --key KEY' on CI
%% servers or similar systems. It returns the hash of the generated key that you can pass to auth NAME `--key' KEY.
%% Unlike the `hex user' key commands, a key generated with this command is owned by the organization directly,
%% and not the user that generated it. This makes it ideal for shared environments such as CI where you don't
%% want to give access to user-specific resources and the user's organization membership status won't affect key. By
%% default this command sets the organization permission which allows read-only access to the organization, it can be
%% overridden with the `--permission' flag.
%%
%%
%% ```
%% $ rebar3 hex organization key NAME generate [--key-name KEY_NAME] [--permission PERMISSION]
%% '''
%%
%% == Revoke key ==
%% Removes a given key from a organization.
%%
%% ```
%% $ rebar3 hex organization key NAME revoke KEY_NAME
%% '''
%%
%% == List keys ==
%% Lists all keys associated with the organization.
%%
%% ```
%% $ rebar3 hex organization key NAME list
%% '''
%%
%% == Command line options ==
%%
%% <ul>
%%  <li>`--all' - Used for revoking all keys for authorized organization. Only valid with the `revoke` task.</li>
%%  <li>`--key KEY' - Hash of key used to authenticate HTTP requests to organization, if omitted will generate a new key
%%  with your account credentials. This flag is useful if you have a key pre-generated with 
%%  `rebar3 hex organization key' and want to authenticate on a CI server or similar system.</li>
%%  <br/>
%% <li>`--key-name KEY_NAME' - By default Hex will base the key name on your machine's hostname and the organization 
%% name, use this option to give your own name.</li>
%%  <br/>
%%  <li>`--permission PERMISSION' -  Sets the permissions on the key, this option can be given multiple times, possibly 
%%       values are:
%%      <ul>
%%          <br/>
%%          <li>`api:read' - API read access.</li>
%%          <li>`api:write' - API write access.</li>
%%          <li>`repository' - Access to the repository (this is the default permission).</li>
%%      </ul>
%%  </li>
%% </ul>

-module(rebar3_hex_organization).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("rebar3_hex.hrl").

-define(PROVIDER, organization).
-define(DEPS, []).

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
        {example, "rebar3 hex organization auth my_org --key 1234"},
        {short_desc, "Add, remove or list configured organizations and their auth keys"},
        {desc, ""},
        {opts, [
            {all, undefined, "all", boolean, "Specifies all keys. Only recognized when used with the revoke task."},
            {key, $k, "key", string, "Authentication key for an organization that already exists at the repository."},
            {key_name, undefined, "key-name", string, "Specifies a key name to use when generating or revoking a key."},
            {permission, $p, "permission", list, "Colon delimited permission. This option may be given multiple times."}
        ]}
    ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["auth", OrgName | _] ->
            auth(State, to_binary(OrgName));
        ["deauth", OrgName | _] ->
            deauth(State, to_binary(OrgName));
        ["key", OrgName, "generate" | _] ->
            generate(State, to_binary(OrgName));
        ["key", OrgName, "revoke", "--all" | _] ->
            revoke_all(State, to_binary(OrgName));
        ["key", OrgName, "revoke" | _] ->
            revoke(State, to_binary(OrgName));
        ["key", OrgName, "list" | _] ->
            list_org_keys(State, to_binary(OrgName));
        ["list" | _] ->
            list_orgs(State);
        _ ->
            ?RAISE(bad_command)
    end.

%% @private
-spec format_error(any()) -> iolist().
format_error(no_repo) ->
    "Authenticate and generate commands require repository name as argument";
format_error(auth_no_key) ->
    "Repo authenticate command requires key";

format_error({auth, Reason}) when is_binary(Reason) ->
    io_lib:format("Error authenticating organization : ~ts", [Reason]);
format_error({auth, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Error authenticating organization : ~ts", [Reason]);

format_error({generate_key, Reason}) when is_binary(Reason) ->
    io_lib:format("Error generating organization key: ~ts", [Reason]);
format_error({generate_key, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Error generating organization key: ~ts", [Reason]);

format_error({key_generate, Reason}) when is_binary(Reason) ->
    io_lib:format("Error generating organization key: ~ts", [Reason]);
format_error({key_generate, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Error generating organization key: ~ts", [Reason]);

format_error({key_revoke_all, Reason}) when is_binary(Reason) ->
    io_lib:format("Error revoking all organization keys: ~ts", [Reason]);
format_error({key_revoke_all, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Error revoking all organization keys: ~ts", [Reason]);

format_error({key_list, Reason}) when is_binary(Reason) ->
    io_lib:format("Error listing organization keys: ~ts", [Reason]);
format_error({key_list, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Error listing organization keys: ~ts", [Reason]);

format_error(bad_command) ->
    "Invalid arguments, expected one of:\n\n"
    "rebar3 hex organization auth ORG_NAME auth\n"
    "rebar3 hex organization deauth ORG_NAME deauth\n"
    "rebar3 hex organization key ORG_NAME generate\n"
    "rebar3 hex organization key ORG_NAME revoke --key-name NAME\n"
    "rebar3 hex organization key ORG_NAME revoke --all\n"
    "rebar3 hex organization key ORG_NAME list\n"
    "rebar3 hex organization list\n";

format_error(not_a_valid_repo_name) ->
    "Invalid organization repository: organization name arguments must be given as a fully qualified "
    "repository name (i.e, hexpm:my_org)";

format_error({get_parent_repo_and_org_name, Error, Name}) -> 
    Str = "Error getting the parent repo for ~ts. Be sure to authenticate first with: rebar3 hex user",
    rebar_log:log(diagnostic, "Error getting parent repo and org name: ~p", [Error]),
    io_lib:format(Str, [Name]);
format_error({get_repo_by_name, {error,{not_valid_repo,ParentName}}}) ->
    Str = io_lib:format("You do not appear to be authenticated as a user to the ~ts repository.", [ParentName]),
    Str ++  " " ++ "Run rebar3 hex user auth and try this command again.";

format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

-dialyzer({nowarn_function, auth/2}).
-spec auth(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
auth(State, RepoName) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    {ParentRepo, OrgName} = get_parent_repo_and_org_name(State, RepoName),
    Key =
        case proplists:get_value(key, Opts, undefined) of
            undefined ->
                Config = rebar3_hex_config:get_hex_config(?MODULE, ParentRepo, write),
                Config1 = Config#{api_organization => OrgName},
                KeyName = proplists:get_value(key_name, Opts, rebar3_hex_config:repos_key_name()),
                generate_key(Config1, KeyName, default_perms(OrgName));
            ProvidedKey ->
                TestPerms = #{domain => <<"repository">>, resource => OrgName},
                Config = ParentRepo#{api_key => to_binary(ProvidedKey), 
                                     api_repository => OrgName, 
                                     api_organization => OrgName
                                    },
                case rebar3_hex_client:test_key(Config, TestPerms) of
                    {ok, _} ->
                        ProvidedKey;
                    Error ->
                        ?RAISE({auth, Error})
                end
        end,
    rebar3_hex_config:update_auth_config(#{RepoName => #{name => RepoName, repo_key => Key}}, State),
    rebar3_hex_io:say("Successfully authenticated to ~ts", [RepoName]),
    {ok, State}.

-spec deauth(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
deauth(State, RepoName) ->
    ok = rebar_hex_repos:remove_from_auth_config(RepoName, State),
    rebar3_hex_io:say("Successfully deauthorized ~ts", [RepoName]),
    {ok, State}.

-spec generate(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
generate(State, RepoName) ->
    {Repo, OrgName} = get_parent_repo_and_org_name(State, RepoName),
    {Opts, _} = rebar_state:command_parsed_args(State),
    KeyName = proplists:get_value(key_name, Opts, rebar3_hex_config:repos_key_name()),
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    PermOpts = proplists:get_all_values(permission, Opts),
    Perms = rebar3_hex_key:convert_permissions(PermOpts, default_perms(OrgName)),
    Key = generate_key(Config#{api_organization => OrgName}, KeyName, Perms),
    rebar3_hex_io:say("~ts", [Key]),
    {ok, State}.

-spec list_org_keys(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
list_org_keys(State, RepoName) ->
    {Repo, OrgName} = get_parent_repo_and_org_name(State, RepoName),
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, read),
    case rebar3_hex_key:list(Config#{api_organization => OrgName}) of
        ok ->
            {ok, State};
        {error, #{<<"errors">> := Errors}} ->
            ?RAISE({key_list, Errors});
        {error, #{<<"message">> := Message}} ->
            ?RAISE({key_list, Message});
        Error ->
            ?RAISE({key_list, Error})
    end.

-spec revoke(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
revoke(State, RepoName) ->
    {Repo, OrgName} = get_parent_repo_and_org_name(State, RepoName),
    {Opts, _} = rebar_state:command_parsed_args(State),
    KeyName = case proplists:get_value(key_name, Opts, undefined) of
                  undefined ->
                      ?RAISE(bad_command);
                  K ->
                      K
              end,
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    case rebar3_hex_key:revoke(Config#{api_organization => OrgName}, KeyName) of
        ok ->
            rebar3_hex_io:say("Key successfully revoked", []),
            {ok, State};
        {error, #{<<"errors">> := Errors}} ->
            ?RAISE({key_revoke, Errors});
        {error, #{<<"message">> := Message}} ->
            ?RAISE({key_revoke, Message});
        Error ->
            ?RAISE({key_revoke, Error})
    end.

-spec revoke_all(rebar_state:t(), binary()) -> {ok, rebar_state:t()}.
revoke_all(State, RepoName) ->
    {Repo, OrgName} = get_parent_repo_and_org_name(State, RepoName),
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    case rebar3_hex_key:revoke_all(Config#{api_organization => OrgName}) of
        ok ->
            rebar3_hex_io:say("All keys successfully revoked", []),
            {ok, State};
        {error, #{<<"errors">> := Errors}} ->
            ?RAISE({key_revoke_all, Errors});
        {error, #{<<"message">> := Message}} ->
            ?RAISE({key_revoke_all, Message});
        Error ->
            ?RAISE({key_revoke_all, Error})
    end.

-spec list_orgs(rebar_state:t()) -> {ok, rebar_state:t()}.
list_orgs(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Headers = ["Name", "URL", "Public Key"],
    Orgs = lists:foldl(
        fun(#{name := Name} = Repo, Acc) ->
            case binary:split(Name, <<":">>) of
                [_, _] ->
                    [Repo | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Repos
    ),

    Rows = lists:map(
        fun(Repo) ->
            #{
                name := Name,
                api_organization := Org,
                repo_url := Url,
                repo_public_key := PubKey
            } = Repo,
            [
                binary_to_list(Name),
                org_url(Org, Url),
                printable_public_key(PubKey)
            ]
        end,
        Orgs
    ),
    rebar3_hex_results:print_table([Headers] ++ Rows),
    {ok, State}.

-spec default_perms(binary()) -> [map()].
default_perms(OrgName) ->
    [#{<<"domain">> => <<"repository">>, <<"resource">> => OrgName}].

-spec generate_key(map(), binary() | undefined, [map()]) -> binary().
generate_key(HexConfig, KeyName, Perms) ->
    case rebar3_hex_key:generate(HexConfig, KeyName, Perms) of
        {ok, #{<<"secret">> := Secret}} ->
            Secret;
        {error, #{<<"errors">> := Errors}} ->
            ?RAISE({generate_key, Errors});
        {error, #{<<"message">> := Message}} ->
            ?RAISE({generate_key, Message});
        Error ->
            ?RAISE({generate_key, Error})
    end.

-spec printable_public_key(binary()) -> nonempty_string().
printable_public_key(PubKey) ->
    [Pem] = public_key:pem_decode(PubKey),
    Public = public_key:pem_entry_decode(Pem),
    Hash = crypto:hash(sha256, ssh_encode(Public)),
    Encoded = string:substr(base64:encode_to_string(Hash), 1, 43),
    "SHA256:" ++ Encoded.


-ifdef(OTP_RELEASE). % OTP >= 21
-if(?OTP_RELEASE >= 24).
-spec ssh_encode(binary()) -> binary().
ssh_encode(InData) ->
    ssh_file:encode(InData, ssh2_pubkey).
-else. % OTP < 24
-spec ssh_encode(binary()) -> binary().
ssh_encode(InData) ->
    public_key:ssh_encode(InData, ssh2_pubkey).
-endif.
-else. % OTP < 21
-spec ssh_encode(binary()) -> binary().
ssh_encode(InData) ->
    public_key:ssh_encode(InData, ssh2_pubkey).
-endif.

to_binary(Name) ->
    rebar_utils:to_binary(Name).

-spec org_url(binary(), binary()) -> [byte(), ...].
org_url(Org, Url) -> binary_to_list(Url) ++ "/repos/" ++ binary_to_list(Org).

get_parent_repo_and_org_name(State, RepoName) ->
    case binary:split(RepoName, <<":">>) of
        [Parent, Org] ->
            case rebar3_hex_config:repo(State, Parent) of
                {ok, Repo} ->
                    {Repo, Org};
                Error ->
                    ?RAISE({get_parent_repo_and_org_name, Error, RepoName})
            end;
        [_] ->
            ?RAISE(not_a_valid_repo_name)
    end.
