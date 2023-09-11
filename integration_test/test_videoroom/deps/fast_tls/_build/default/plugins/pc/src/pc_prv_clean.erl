%% -------------------------------------------------------------------
%%
%% rebar3 port compiler cleaner, in the manner of rebar2.
%%
%% -------------------------------------------------------------------
-module(pc_prv_clean).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, pc).
-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},             %% The 'user friendly' name of the task
                                 {module, ?MODULE},             %% The module implementation of the task
                                 {bare, true},                  %% The task can be run by the user, always true
                                 {deps, ?DEPS},                 %% The list of dependencies
                                 {example, "rebar pc clean"},   %% How to use the plugin
                                 {opts, []},                    %% list of options understood by the plugin
                                 {short_desc, "clean the results of port compilation"},
                                 {desc, ""},
                                 {namespace, pc}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(
      Cwd, pre, {?NAMESPACE, ?PROVIDER}, Providers, State),

    {ok, PortSpecs} = pc_port_specs:construct(State),
    ok = pc_compilation:clean(State, PortSpecs),

    rebar_hooks:run_all_hooks(
      Cwd, post, {?NAMESPACE, ?PROVIDER}, Providers, State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
