%% @private
-module(rebar3_hex_config).

-export([ api_key_name/1
        , api_key_name/2
        , all_repos/1
        , encrypt_write_key/3
        , decrypt_write_key/3
        , repos_key_name/0
        , org_key_name/2
        , parent_repos/1
        , get_hex_config/3
        , default_repo/1
        , repo/1
        , repo/2
        , update_auth_config/2
        ]).

-include("rebar3_hex.hrl").

-type repo_error() :: {not_valid_repo, string()} | no_repo_in_state | {required, repo}.
-export_type([repo_error/0]).

-spec api_key_name(binary()) -> binary().
api_key_name(Key) ->
    Prefix = key_name_prefix(Key),
    key_name(Prefix, <<"-api">>).

-spec api_key_name(binary(), binary()) -> binary().
api_key_name(Key, Suffix) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-api-">>, Suffix).

-ifdef(POST_OTP_22).
-spec encrypt_write_key(binary(), binary(), binary()) -> {binary(), {binary(), binary()}}.
encrypt_write_key(Username, LocalPassword, WriteKey) ->
    AAD = Username,
    IV = crypto:strong_rand_bytes(16),
    Key =  pad(LocalPassword),
    {IV, crypto:crypto_one_time_aead(cipher(Key), Key, IV, WriteKey, AAD, true)}.
-else.
-spec encrypt_write_key(binary(), binary(), binary()) -> {binary(), {binary(), binary()}}.
encrypt_write_key(Username, LocalPassword, WriteKey) ->
    AAD = Username,
    IV = crypto:strong_rand_bytes(16),
    {IV, crypto:block_encrypt(aes_gcm, pad(LocalPassword), IV, {AAD, WriteKey})}.
-endif.

-ifdef(POST_OTP_22).
decrypt_write_key(Username, LocalPassword, {IV, {CipherText, CipherTag}}) ->
    Key = pad(LocalPassword),
    crypto:crypto_one_time_aead(cipher(Key), Key, IV, CipherText, Username, CipherTag, false).
-else.
decrypt_write_key(Username, LocalPassword, {IV, {CipherText, CipherTag}}) ->
    crypto:block_decrypt(aes_gcm, pad(LocalPassword), IV, {Username, CipherText, CipherTag}).
-endif.

-ifdef(POST_OTP_22).
cipher(Key) when byte_size(Key) == 16  -> aes_128_gcm;
cipher(Key) when byte_size(Key) == 24  -> aes_192_gcm;
cipher(Key) when byte_size(Key) == 32  -> aes_256_gcm.
-endif.

pad(Binary) ->
    case byte_size(Binary) of
        Size when Size =< 16 ->
            <<Binary/binary, 0:((16 - Size) * 8)>>;
        Size when Size =< 24 ->
            <<Binary/binary, 0:((24 - Size) * 8)>>;
        Size when Size =< 32 ->
            <<Binary/binary, 0:((32 - Size) * 8)>>
    end.

-spec repos_key_name() -> binary().
repos_key_name() ->
     key_name(hostname(), <<"-repositories">>).

-spec org_key_name(binary(), binary()) -> binary().
org_key_name(Key, Org) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-repository-">>, Org).

-spec hostname() -> binary().
hostname() ->
    {ok, Name} = inet:gethostname(),
    list_to_binary(Name).

key_name(Prefix, Suffix) ->
    <<Prefix/binary, Suffix/binary>>.

key_name(Prefix, Interfix, Suffix) ->
    <<Prefix/binary, Interfix/binary, Suffix/binary>>.

key_name_prefix(undefined) -> hostname();
key_name_prefix(Key) -> Key.

update_auth_config(Config, State) ->
    rebar_hex_repos:update_auth_config(Config, State).

all_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Repos.

-spec repo(rebar_state:t()) -> {ok, map()} | {error, repo_error()}.
repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Repos = all_repos(State),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            Res = [R || R <- Repos, maps:get(name, R) =/= ?DEFAULT_HEX_REPO],
            case Res of
                [] ->
                    case rebar_hex_repos:get_repo_config(?DEFAULT_HEX_REPO, Repos) of
                        {ok, Repo} ->
                            {ok, set_http_adapter(Repo)};
                        _ ->
                            {error, no_repo_in_state}
                    end;
                [_Repo|_Rest] ->
                    {error, {required, repo}}
            end;
        RepoName ->
            repo(State, RepoName)
    end.

repo(State, RepoName) ->
    BinName = rebar_utils:to_binary(RepoName),
    Repos = all_repos(State),
    MaybeFound1 = get_repo(BinName, all_repos(State)),
    MaybeParentRepo = <<"hexpm:">>,
    MaybeFound2 =  get_repo(<<MaybeParentRepo/binary, BinName/binary>>, Repos),
    case {MaybeFound1, MaybeFound2} of
        {{ok, Repo1}, undefined} ->
            Repo2 = set_http_adapter(merge_with_env(Repo1)),
            {ok, maybe_set_api_organization(Repo2)};
        {undefined, {ok, Repo2}} ->
            Repo3 = set_http_adapter(merge_with_env(Repo2)),
            {ok, maybe_set_api_organization(Repo3)};
        {undefined, undefined} ->
            {error, {not_valid_repo, RepoName}}
    end.


-define( ENV_VARS
       , [ {"HEX_API_KEY", {api_key, {string, undefined}}}
         , {"HEX_API_URL", {api_url, {string, undefined}}}
         , {"HEX_UNSAFE_REGISTRY", {repo_verify, {boolean, false}}}
         , {"HEX_NO_VERIFY_REPO_ORIGIN", {repo_verify_origin, {boolean, true}}}
         ]
       ).

merge_with_env(Repo) ->
    lists:foldl(fun({EnvName, {Key, _} = Default}, Acc) ->
                        Val = maybe_env_val(EnvName, Default),
                        maybe_put_key(Key, Val, Acc)
                end, Repo, ?ENV_VARS).

maybe_put_key(_Key, undefined, Repo) ->
    Repo;
maybe_put_key(Key, Val, Repo) ->
    case maps:get(Key, Repo, undefined) of
        Val ->
            Repo;
        _ ->
            Repo#{Key => Val}
    end.

maybe_env_val(K, {_, {Type, Default}}) ->
    case {os:getenv(K), {Type, Default}} of
        {false, {_, Default}} ->
            Default;
        {"", {_, Default}} ->
            Default;
        {Val, {boolean, _}} ->
            to_bool(string:to_lower(Val));
        {Val, {string, _}} ->
          rebar_utils:to_binary(Val)
    end.

set_http_adapter(Repo) ->
    Repo#{http_adapter => {rebar3_hex_httpc_adapter, #{profile => rebar}}}.

to_bool("0") -> false;
to_bool("false") -> false;
to_bool(_) -> true.

parent_repos(State) ->
    Fun = fun(#{name := Name} = Repo, Acc) ->
                  [Parent|_] = rebar3_hex_io:str_split(Name, <<":">>),
                  case maps:is_key(Parent, Acc) of
                    true ->
                        Acc;
                    false ->
                        maps:put(name, Repo, Acc)
                  end
          end,
    Map = lists:foldl(Fun, #{}, all_repos(State)),
    maps:values(Map).

default_repo(State) ->
    rebar_hex_repos:get_repo_config(?DEFAULT_HEX_REPO, all_repos(State)).

get_repo(BinaryName, Repos) ->
    try rebar_hex_repos:get_repo_config(BinaryName, Repos) of
        Name ->
            Name
    catch
        {error,{rebar_hex_repos,{repo_not_found,BinaryName}}} -> undefined
    end.

-spec get_hex_config(module(), map(), read | write) -> map().
get_hex_config(Module, Repo, Mode) ->
    case hex_config(Repo, Mode) of
        {ok, HexConfig} ->
            HexConfig;
        {error, Reason} ->
            erlang:error({error, {Module, {get_hex_config, Reason}}})
    end.

hex_config(Repo, read) ->
    hex_config_read(Repo);
hex_config(Repo, write) ->
    hex_config_write(Repo).

hex_config_write(#{api_key := Key} = HexConfig) when is_binary(Key) ->
    {ok, set_http_adapter(HexConfig)};
hex_config_write(#{write_key := undefined}) ->
    {error, no_write_key};
hex_config_write(#{write_key := WriteKey, username := Username} = HexConfig) ->
    DecryptedWriteKey = rebar3_hex_user:decrypt_write_key(Username, WriteKey),
    {ok, set_http_adapter(HexConfig#{api_key => DecryptedWriteKey})};
hex_config_write(_) ->
    {error, no_write_key}.

hex_config_read(#{read_key := ReadKey} = HexConfig) ->
    {ok, set_http_adapter(HexConfig#{api_key => ReadKey})};
hex_config_read(_Config) ->
    {error, no_read_key}.

maybe_set_api_organization(#{name := Name} = Repo) ->
    case binary:split(Name, <<":">>) of
        [_] ->
            Repo;
        [_,Org] ->
            Repo#{api_organization => Org}
    end.
