%% @private
-module(rebar3_hex_key).

-export([convert_permissions/2, fetch/2, format_error/1, generate/3, revoke/2, revoke_all/1, list/1]).

generate(HexConfig, KeyName, Perms) ->
    case rebar3_hex_client:key_add(HexConfig, KeyName, Perms) of
      {ok, _Res} = Ret ->
        Ret;
      Error ->
        Error
    end.

fetch(HexConfig, KeyName) ->
    case rebar3_hex_client:key_get(HexConfig, KeyName) of
      {ok, Res} ->
        print_key_details(Res);
      Error ->
        Error
    end.

revoke(HexConfig, KeyName) ->
    case rebar3_hex_client:key_delete(HexConfig, KeyName) of
      {ok, _Res} ->
        ok;
      Error ->
        Error
    end.

revoke_all(HexConfig) ->
    case rebar3_hex_client:key_delete_all(HexConfig) of
      {ok, _Res} ->
          ok;
      Error ->
        Error
    end.

list(HexConfig) ->
    case rebar3_hex_client:key_list(HexConfig) of
      {ok, Res} ->
        print_results(Res);  
      Error ->
        Error
    end.

-spec convert_permissions([string()], [map()]) -> [map()].
convert_permissions([], Defaults) -> 
    Defaults;
convert_permissions(Perms, _) -> 
    lists:foldl(fun (Name, Acc) ->
                             case binary:split(rebar_utils:to_binary(Name), <<":">>) of 
                                 [Domain] -> 
                                     [#{<<"domain">> => Domain, <<"resource">> => nil}] ++ Acc;

                                [Domain, Resource] -> 
                                    [#{<<"domain">> => Domain, <<"resource">> => Resource}] ++ Acc
                             end
                end,
                [],
                Perms).

print_results(Res) ->
    Header = ["Name", "Created"],
    Rows = lists:map(fun (#{<<"name">> := Name, <<"inserted_at">> := Created}) ->
                             [binary_to_list(Name), binary_to_list(Created)]
                     end,
                     Res),
    ok = rebar3_hex_results:print_table([Header] ++ Rows),
    ok.



print_key_details(#{<<"name">> := Name,
                    <<"inserted_at">> := Created,
                    <<"updated_at">> := Updated,
                    <<"last_use">> :=
                        #{<<"ip">> := Addr, <<"used_at">> := Used, <<"user_agent">> := _Agent}}) ->
    Header = ["Name", "Created", "Updated", "LastUsed", "LastUsedBy"],
    Row = [binary_to_list(Name),
           binary_to_list(Created),
           binary_to_list(Updated),
           binary_to_list(Used),
           binary_to_list(Addr)],
    ok = rebar3_hex_results:print_table([Header] ++ [Row]),
    ok;

print_key_details(#{<<"name">> := Name,
                    <<"inserted_at">> := Created,
                    <<"updated_at">> := Updated}) ->
    Header = ["Name", "Created", "Updated", "LastUsed", "LastUsedBy"],
    Row = [binary_to_list(Name),
           binary_to_list(Created),
           binary_to_list(Updated),
           "never",
           "n/a"],
    ok = rebar3_hex_results:print_table([Header] ++ [Row]),
    ok.

-spec format_error(any()) -> iolist().
format_error({list, {unauthorized, _Res}}) ->
    "Error while attempting to perform list : Not authorized";
format_error({list, {error, #{<<"message">> := Msg}}}) ->
    "Error while attempting to perform list : " ++ Msg;
format_error({revoke, {not_found, _Res}}) ->
    "Error while revoking key : key not found";
format_error({generate,
              {validation_errors, #{<<"errors">> := Errors, <<"message">> := Message}}}) ->
    ErrorString = rebar3_hex_results:errors_to_string(Errors),
    io_lib:format("~ts~n\t~ts", [Message, ErrorString]);
format_error(bad_command) ->
    "Unknown command. Command must be fetch, generate, list, or "
    "revoke";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

