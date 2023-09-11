%% @private
-module(rebar3_hex_error).

-export([format_error/1]).

format_error({required, repo}) ->
    "A repository argument is required for this command.";

format_error({not_valid_repo, RepoName}) ->
    io_lib:format("Could not find ~ts in repo configuration. Be sure to authenticate first with rebar3 hex user auth.",
                  [RepoName]);

format_error({get_hex_config, no_read_key}) ->
    "No read key found for user. Be sure to authenticate first with:"
    " rebar3 hex user auth";

format_error({get_hex_config, no_write_key}) ->
    "No write key found for user. Be sure to authenticate first with:"
    " rebar3 hex user auth";

format_error({Cmd, unsupported_params}) ->
    io_lib:format("Either some or all of the parameters supplied for the ~ts command are ", [Cmd])
    ++ " invalid or form an invalid combination of parameters.";

format_error({Cmd, missing_required_params}) ->
    io_lib:format("Required parameters for the ~ts command have not been supplied.", [Cmd]);

format_error({Cmd, {error, {failed_connect, [{to_address,_},
                                        {inet,[inet],econnrefused}]} = Err}}) ->
    rebar_log:log(diagnostic, "Failed to connect to hex repository : ~p , context : ~ts", [Err, Cmd]),
    io_lib:format("Error connecting to hex repository api. Run with DIAGNOSTIC=1 for more details", []);

format_error(Reason) ->
    rebar_log:log(diagnostic, "Unknown error: ~p", [Reason]),
    "An unknown error was encountered. Run with DIAGNOSTIC=1 for more details.".
