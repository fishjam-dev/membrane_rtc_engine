%% @doc `rebar3 hex retire' - retire and unretire packages
%%
%% Retires a package version.
%%
%% ```
%% $ rebar3 hex retire PACKAGE VERSION REASON --message
%% $ rebar3 hex retire PACKAGE VERSION --unretire
%% '''
%%
%% Mark a package as retired when you no longer recommend it's usage. A retired package is still resolvable and usable 
%% but it will be flagged as retired in the repository and a message will be displayed to users when they use the 
%% package.
%%
%% <h2> Retirement reasons </h2>
%%
%% <ul>
%%   <li><b>renamed</b> - The package has been renamed, including the new package name in the message.</li>
%%   <li><b>deprecated</b> - The package has been deprecated, if there's a replacing package include it in the message</li>
%%   <li><b>security</b> - There are security issues with this package</li>
%%   <li><b>invalid</b> - The package is invalid, for example it does not compile correctly</li>
%%   <li><b>other</b> - Any other reason not included above, clarify the reason in the message</li>
%%  </ul>
%%
%% <h2> Command line options </h2>
%%
%% <ul>
%%  <li>`--repo' - Specify the repository to work with. This option is required when 
%%      you have multiple repositories configured, including organizations. The argument must 
%%      be a fully qualified repository name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%   </li>
%% </ul>

-module(rebar3_hex_retire).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, retire).
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
                                 {example, "rebar3 hex retire some_pkg 0.3.0 invalid --message Clarifying message"},
                                 {short_desc, "Mark a package as deprecated."},
                                 {desc, ""},
                                 {opts, [
                                         {message, $m, "message", string, "Clarifying message for retirement"},
                                         rebar3_hex:repo_opt()]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, {?MODULE, rebar3_hex_config:repo_error()}}.
do(State) ->
        case rebar3_hex_config:repo(State) of
            {ok, Repo} ->
                case rebar_state:command_args(State) of
                    [Pkg, Version, Reason|_] ->
                        {Opts, _} = rebar_state:command_parsed_args(State),
                        Message = rebar_utils:to_binary(get_required_or_raise(message, Opts)),
                        PkgName = rebar_utils:to_binary(Pkg),
                        VersionBin = rebar_utils:to_binary(Version),
                        ReasonBin = rebar_utils:to_binary(Reason),
                        MessageBin = rebar_utils:to_binary(Message),
                        retire(State, PkgName, VersionBin, Repo, ReasonBin, MessageBin);
                    _ ->
                        ?RAISE(bad_command)
        end;

        {error, Reason} ->
            ?RAISE(Reason)
    end.

get_required_or_raise(Key, Args) ->
    case rebar3_hex:get_required(Key, Args) of
            {error, Err} ->
                ?RAISE(Err);
            Res ->
                Res
    end.

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string(Map) when is_map(Map) ->
    errors_to_string(maps:to_list(Map));
errors_to_string({<<"reason">> = Key, <<"is invalid">> = Value}) ->
    ValidVals =  "must be one of other, invalid, security, deprecated or renamed",
	io_lib:format("~s: ~s - ~s", [Key, errors_to_string(Value),  ValidVals]);
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

%% @private
format_error({validation_errors, Errors, Message}) ->
    ErrorString = errors_to_string(Errors),
    io_lib:format("Failed to retire package: ~ts~n\t~ts", [Message, ErrorString]);
format_error({api_error, PkgName, Version, Reason}) ->
    io_lib:format("Unable to delete package ~ts ~ts: ~ts", [PkgName, Version, Reason]);
format_error({required, pkg}) ->
    "retire requires a package name argument to identify the package to delete";
format_error({required, vsn}) ->
    "retire requires a version number argument to identify the package to delete";
format_error({required, reason}) ->
    "retire requires a reason with value of either other, invalid, security, deprecated or renamed";
format_error({required, message}) ->
    "retire requires a message to clarify the reason for the retirement of the package";
format_error(bad_command) ->
    "Invalid arguments, expected one of:\n\n"
    "rebar3 hex retire PACKAGE VERSION REASON --message MESSAGE\n";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

retire(State, PkgName, Version, Repo, RetireReason, RetireMessage) ->
    HexConfig = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    Msg = #{<<"reason">> => RetireReason,
                <<"message">> => RetireMessage},
    case hex_api_release:retire(HexConfig, PkgName, Version, Msg) of
        {ok, {204, _Headers, _Body}} ->
            rebar_api:info("Successfully retired package ~ts ~ts", [PkgName, Version]),
            {ok, State};
        {ok, {422, _Headers, #{<<"errors">> := Errors, <<"message">> := Message}}} ->
            ?RAISE({validation_errors, Errors, Message});
        {ok, {Code, _Headers, _Body}} ->
            ?RAISE({api_error, PkgName, Version, rebar3_hex_client:pretty_print_status(Code)});
        {error, Reason} ->
            ?RAISE({api_error, PkgName, Version, io_lib:format("~p", [Reason])})
    end.
