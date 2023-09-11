%% @doc `rebar3 hex owner'  - Manage package owners
%%
%% Adds, removes or lists package owners.
%% 
%% Package owners have full permissions to the package. They can publish and revert releases and even remove 
%% other package owners.
%%
%% <h2>Add a owner</h2>
%% Adds an owner to package by specifying the package name and email or username of the new owner.
%% ```
%% $ rebar3 hex owner add PACKAGE EMAIL_OR_USERNAME
%% '''
%%
%% <h2>Transfer ownership</h2>
%% Like `rebar3 hex owner add` add but also removes all existing owners of the package. This task is required to use 
%% when transferring ownership of the package to an organization.
%%
%% ```
%% $ rebar3 hex owner transfer PACKAGE EMAIL_OR_USERNAME
%% '''
%%
%% <h2>Remove owner</h2>
%% 
%% Removes an owner to package by specifying the package name and email or username of the new owner.
%%
%% ```
%% $ rebar3 hex owner remove PACKAGE EMAIL_OR_USERNAME
%% '''
%%
%% <h2> List owners </h2>
%% Lists all owners of given package.
%%
%% ```
%% $ rebar3 hex owner list PACKAGE
%% '''
%%
%% <h2> List owned packages </h2>
%% Lists all packages owned by the current user.
%%
%% ```
%% $ rebar3 hex owner list packages
%% '''
%%
%% <h2> Command line options </h2>
%%
%% <ul>
%%  <li>`--repo' - Specify the repository to work with. This option is required when 
%%      you have multiple repositories configured, including organizations. The argument must 
%%      be a fully qualified repository name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%      Defaults to `hexpm'.
%%   </li>
%%  <li>`--level' - Specify the ownership level, either `full' or `maintainer'. Both ownership levels allow publishing 
%%      and retiring of packages. However, only an owner with `full' access may add, remove other owners, or transfer a 
%%      package to another owner. 
%%      Defaults to `full'.
%%   </li>
%% </ul>

-module(rebar3_hex_owner).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, owner).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex owner"},
                                 {short_desc, "Add, remove, transfer or list package owners"},
                                 {desc, support()},
                                 {opts, [rebar3_hex:repo_opt(),
                                         {level, $l, "level", {string, "full"}, "Ownership level."},
                                         {transfer, $t, "transfer", {boolean, false}, "Transfer Package"}
                                        ]}]),

    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar3_hex_config:repo(State) of
        {ok, Repo} ->
            {ok, handle_command(State, Repo)};
        {error, Reason} ->
            ?RAISE(Reason)
    end.

handle_command(State, Repo) ->
    case command_args(State) of
        {"add", Package, UsernameOrEmail, Level, Transfer} ->
            case valid_level(Level) of
                true ->
                    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
                    add(Config, Package, UsernameOrEmail, Level, Transfer, State),
                    ok = rebar3_hex_io:say("Added ~ts to ~ts", [UsernameOrEmail, Package]),
                    State;
                false ->
                    ?RAISE({error, "level must be one of full or maintainer"})
            end;
        {"remove", Package, UsernameOrEmail} ->
            Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
            remove(Config, Package, UsernameOrEmail, State),
            ok = rebar3_hex_io:say("Removed ~ts to ~ts", [UsernameOrEmail, Package]),
            State;
        {"transfer", Package, UsernameOrEmail} ->
            Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
            add(Config, Package, UsernameOrEmail, <<"full">>, true, State),
            ok = rebar3_hex_io:say("Transferred ~ts to ~ts", [Package, UsernameOrEmail]),
            State;
        {"list", Package} ->
            Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, read),
            list(Config, Package, State);
        _Command ->
            ?RAISE(bad_command)
    end.

command_args(State) ->
    case get_args(rebar_state:command_args(State)) of
        {"list", Package} ->
            {"list", rebar_utils:to_binary(Package)};

        {"add", Package, UserOrEmail} ->
            {AllArgs, _} = rebar_state:command_parsed_args(State),
            Level = proplists:get_value(level, AllArgs, "full"),
            Transfer = proplists:get_value(transfer, AllArgs, false),
            {"add", rebar_utils:to_binary(Package), rebar_utils:to_binary(UserOrEmail), rebar_utils:to_binary(Level), Transfer};

        {Command, Package, UserOrEmail} ->
            {Command, rebar_utils:to_binary(Package), rebar_utils:to_binary(UserOrEmail)};

        BadCommand ->
          BadCommand
     end.

get_args(["list", Package]) ->
    {"list", Package};
get_args(["list", Package| _Rest]) ->
    {"list", Package};
get_args([Task, Package, Username]) when Task =:= "transfer" ->
    {Task, Package, Username};
get_args([Task, Package, UserName | _Rest]) when Task =:= "add" orelse Task =:= "remove" ->
    {Task, Package, UserName};
get_args([Task, Package, UserName, "-r", _]) ->
    {Task, Package, UserName};
get_args(BadCommand) ->
    BadCommand.

support() ->
    "Adds, removes or lists package owners.~n~n"
    "Package owners have full permissions to the package. They can "
    "publish and revert releases and even remove other package owners.~n~n"
    "Supported command combinations: ~n~n"
    "  rebar3 hex owner add <package> <username>~n~n"
    "  rebar3 hex owner add <package> <username> --level <level>~n~n"
    "  rebar3 hex owner list <package>~n~n"
    "  rebar3 hex owner remove <package> <username>~n~n"
    "  rebar3 hex owner transfer <package> <username>~n~n"
    "Argument descriptions: ~n ~n"
    "  <username> - a valid hex username or email address for a hex user~n~n"
    "  <package>  - a valid hex package name~n~n"
    "  <level>    - one of full or maintainer~n~n".

%% @private
-spec format_error(any()) -> iolist().
format_error(bad_command) ->
    S = "Invalid command ~n~n",
    support(),
    io_lib:format(S, []);
format_error({validation_errors, Cmd, Package, User, Errors, Message}) ->
    ErrorString = rebar3_hex_results:errors_to_string(Errors),
    Action = verb_to_gerund(Cmd),
    io_lib:format("Error ~ts ~ts as owner of package ~ts : ~ts~n\t~ts", [Action, User, Package, Message, ErrorString]);
format_error({error, Package, Reason}) ->
    io_lib:format("Error listing owners of package ~ts: ~p", [Package, Reason]);
format_error({status, Status, Package}) ->
    io_lib:format("Error listing owners of package ~ts: ~ts",
                  [Package, rebar3_hex_client:pretty_print_status(Status)]);
format_error({error, Package, UsernameOrEmail, Reason}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~p", [UsernameOrEmail, Package, Reason]);
format_error({status, Status, Package, UsernameOrEmail}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~ts",
                  [UsernameOrEmail, Package, rebar3_hex_client:pretty_print_status(Status)]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

valid_level(<<"full">>) -> true;
valid_level(<<"maintainer">>) -> true;
valid_level(_) -> false.

add(HexConfig, Package, UsernameOrEmail, Level, Transfer, State) ->
    case hex_api_package_owner:add(HexConfig, Package, UsernameOrEmail, Level, Transfer) of
        {ok, {Code, _Headers, _Body}} when Code =:= 204 orelse Code =:= 201->
            State;
		{ok, {422, _Headers, #{<<"errors">> := Errors, <<"message">> := Message}}} ->
            ?RAISE({validation_errors, add, Package, UsernameOrEmail, Errors, Message});
        {ok, {Status, _Headers, _Body}} ->
            ?RAISE({status, Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?RAISE({error, Package, UsernameOrEmail, Reason})
    end.

remove(HexConfig, Package, UsernameOrEmail, State) ->
    case hex_api_package_owner:delete(HexConfig, Package, UsernameOrEmail) of
        {ok, {204, _Headers, _Body}} ->
            State;
        {ok, {Status, _Headers, _Body}} ->
            ?RAISE({status, Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?RAISE({error, Package, UsernameOrEmail, Reason})
    end.

list(HexConfig, Package, State) ->
    case hex_api_package_owner:list(HexConfig, Package) of
        {ok, {200, _Headers, List}} ->
            Owners = [owner(Owner) || Owner <- List],
            OwnersString = rebar_string:join(Owners, "\n"),
            rebar3_hex_io:say("~s", [OwnersString]),
            State;
        {ok, {Status, _Headers, _Body}} ->
            ?RAISE({status, Status, Package});
        {error, Reason} ->
            ?RAISE({error, Package, Reason})
    end.

owner(Owner) ->
    Name0 = maps:get(<<"username">>, Owner, nil),
    Email0 = maps:get(<<"email">>, Owner, nil),
    {Name, Email} = case {Name0, Email0} of
                        _ when is_binary(Name0), is_binary(Email0) ->
                            {Name0, Email0};
                        _ when is_binary(Name0) ->
                            {Name0, <<"unspecified">>};
                        _ when is_binary(Email0) ->
                            {<<"unspecified">>, Email0};
                        _ ->
                            {<<"unspecified">>, <<"unspecified">>}
                    end,
    binary_to_list(Name) ++ " (" ++ binary_to_list(Email) ++ ")".

verb_to_gerund(add) -> "adding";
verb_to_gerund(remove) -> "removing";
verb_to_gerund(list) -> "listing".
