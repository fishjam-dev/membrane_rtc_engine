%% @private
-module(rebar3_hex).

-export([ init/1
        , get_args/1
        , gather_opts/2
        , get_required/2
        , task_args/1
        , task_state/1
        , task_state/2
        , repo_opt/0
        , help_opt/0
        ]).

-export([to_atom/1, to_list/1, to_binary/1]).

-type task() :: #{raw_opts := proplists:proplist(),
                  raw_args := list(),
                  args := map(),
                  repo := map(),
                  apps := [rebar_app_info:t()],
                  state := rebar_state:t(),
                  multi_app := boolean()
                 }.

-export_type([task/0]).

init(State) ->
    lists:foldl(fun provider_init/2, {ok, State}, [rebar3_hex_user,
                                                   rebar3_hex_build,
                                                   rebar3_hex_cut,
                                                   rebar3_hex_owner,
                                                   rebar3_hex_organization,
                                                   rebar3_hex_search,
                                                   rebar3_hex_retire,
                                                   rebar3_hex_publish]).

provider_init(Module, {ok, State}) ->
    Module:init(State).

gather_opts(Targets, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    lists:foldl(fun(T, Acc) ->
                        case proplists:get_value(T, Args, undefined) of
                            undefined ->
                                Acc;
                            V ->
                                maps:put(T, V, Acc)
                        end
                end, #{}, Targets).

get_required(Key, Args) ->
    case proplists:get_value(Key, Args) of
        undefined ->
            {error, {required, Key}};
        Value ->
            Value
    end.

task_args(State) ->
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    case proplists:get_value(task, Opts, undefined) of
        undefined ->
            {undefined, Opts};
        Task ->
            {Task, proplists:delete(task, Opts)}
    end.


%% TODO: A current limitation of rebar3_hex:task_state/1 is it returns strings.
%% This will need to be adjusted to return binaries. For now, we return the opts and args
%% as returned from rebar_state:command_parsed_args/1 as raw_args and raw_opts.
-spec task_state(rebar_state:t()) -> {ok, task()} | {error, term()}.
task_state(State) ->
     case rebar3_hex_config:repo(State) of
         {ok, Repo} ->
             {ok, task_state(State, Repo)};
        Err ->
            Err
     end.

-spec task_state(rebar_state:t(), map()) -> task().
task_state(State, Repo) ->
    {Opts0, Args0} = rebar_state:command_parsed_args(State),
    Opts = get_args(State),
    CurrentTask = maps:get(task, Opts, undefined),
    Opts1 = Opts#{task => CurrentTask},
    case rebar_state:project_apps(State) of
        [] ->
            #{raw_opts => Opts0, raw_args => Args0, args => Opts1, repo => Repo, state => State, multi_app => false, apps => []};
        [_App] = Apps ->
            #{raw_opts => Opts0, raw_args => Args0, args => Opts1, repo => Repo, state => State, multi_app => false, apps => Apps};
        [_|_] = Apps ->
            #{raw_opts => Opts0, raw_args => Args0, args => Opts1, repo => Repo, state => State, multi_app => true, apps => Apps}
     end.

-spec get_args(rebar_state:t()) -> map().
get_args(State) ->
    {Opts, Args} = rebar_state:command_parsed_args(State),
    Opts1 = lists:foldl(fun (Arg, Acc) ->
                                case is_atom(Arg) of
                                    true ->
                                        [{Arg, true} | Acc];
                                    _ ->
                                        case Arg of
                                            {task, Task} ->
                                                [{task, list_to_atom(Task)} | Acc];
                                             _ ->
                                              [Arg | Acc]
                                        end
                                end
                        end,
                        [],
                        Opts),

    Opts2 = lists:foldl(fun (Arg, Acc) ->
                                case is_atom(Arg) of
                                    true ->
                                        [{Arg, true} | Acc];
                                    _ ->
                                     [{list_to_atom(Arg), true} | Acc]
                                end
                        end,
                        Opts1,
                        Args),
    maps:from_list(Opts2).

help_opt() ->
  {help, $h, "help", undefined, "Help"}.

repo_opt() ->
  {repo, $r, "repo", string, "Repository to use for this command."}.

to_binary(A) when is_atom(A) -> atom_to_binary(A, unicode);
to_binary(Str) -> unicode:characters_to_binary(Str).

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> unicode:characters_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(Str) -> unicode:characters_to_list(Str).

to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
to_atom(Str) when is_list(Str) -> list_to_atom(Str);
to_atom(A) when is_atom(A) -> A.
