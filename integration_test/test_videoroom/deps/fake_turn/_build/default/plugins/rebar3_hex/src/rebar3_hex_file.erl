%% @private
-module(rebar3_hex_file).

-include_lib("kernel/include/file.hrl").

-export([
    expand_paths/2,
    update_app_src/2
]).

expand_paths(Paths, Dir) ->
    AbsDir = expand(rebar3_hex:to_binary(Dir)),
    Files1 = [filename:join(AbsDir, rebar3_hex:to_binary(P)) || P <- Paths],
    Files2 = lists:flatmap(
        fun(F) ->
            [rebar3_hex:to_binary(F1) || F1 <- filelib:wildcard(rebar3_hex:to_list(F))]
        end,
        Files1
    ),
    Files3 = lists:flatmap(fun dir_files/1, Files2),
    case lists:map(fun expand/1, Files3) of
        [] ->
            [];
        Files4 ->
            [
             {rebar3_hex:to_list(relative_to(F, AbsDir)), rebar3_hex:to_list(F)} 
             || F <- uniq(Files4)
            ]
    end.

update_app_src(App, Version) ->
    AppSrcFile = rebar_app_info:app_file_src(App),
    AppSrc = rebar_file_utils:try_consult(AppSrcFile),
    [{application, Name, Details}] = AppSrc,
    NewDetails = lists:keyreplace(vsn, 1, Details, {vsn, rebar3_hex:to_list(Version)}),
    {application, Name, NewDetails}.

relative_to(Path, From) when is_binary(Path) andalso is_binary(From) ->
    relative_to(filename:split(Path), filename:split(From), Path).

relative_to(Path1, Path2, _Original) when Path1 =:= Path2 ->
    <<".">>;
relative_to([_H | T1], [_H1 | T2], Original) ->
    relative_to(T1, T2, Original);
relative_to([_ | _] = L1, [], _Original1) ->
    filename:join(L1);
relative_to(_, _, Original) ->
    Original.

dir_files(Path) ->
    case file:read_link_info(Path, [{time, universal}]) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                directory ->
                    {ok, More} = file:list_dir(Path),
                    Paths = [filename:join(Path, F) || F <- More],
                    NewPaths = lists:flatmap(fun dir_files/1, Paths),
                    [Path | NewPaths];
                _ ->
                    [Path]
            end;
        _ ->
            [Path]
    end.

uniq(List) ->
    maps:keys(lists:foldl(fun(E, Acc) -> Acc#{E => true} end, #{}, List)).

expand(Path) ->
    expand_dot(filename:absname(Path)).

expand_dot(<<"/", Rest/binary>>) ->
    <<"/", (do_expand_dot(Rest))/binary>>;
expand_dot(<<Letter/integer, ":/", Rest/binary>>) when is_integer(Letter) andalso Letter >= 97 andalso Letter =< 122 ->
    <<Letter/integer, ":/", (do_expand_dot(Rest))/binary>>;
expand_dot(Path) ->
    do_expand_dot(Path).

do_expand_dot(Path) ->
    do_expand_dot(binary:split(Path, <<"/">>, [global]), []).

do_expand_dot([<<"..">> | T], [_, _ | Acc]) ->
    do_expand_dot(T, Acc);
do_expand_dot([<<"..">> | T], []) ->
    do_expand_dot(T, []);
do_expand_dot([<<".">> | T], Acc) ->
    do_expand_dot(T, Acc);
do_expand_dot([H | T], Acc) ->
    do_expand_dot(T, [<<"/">>, H | Acc]);
do_expand_dot([], []) ->
    <<>>;
do_expand_dot([], [<<"/">> | Acc]) ->
    iolist_to_binary(lists:reverse(Acc)).
