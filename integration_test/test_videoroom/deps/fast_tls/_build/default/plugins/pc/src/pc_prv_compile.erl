%% -------------------------------------------------------------------
%%
%% rebar3 port compiler, in the manner of rebar2.
%%
%% -------------------------------------------------------------------
-module(pc_prv_compile).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, pc).
-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

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
                                 {example, "rebar pc compile"}, %% How to use the plugin
                                 {opts, []},                    %% list of options understood by the plugin
                                 {short_desc, "perform port compilation"},
                                 {desc, ""},
                                 {namespace, pc}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,

    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(
      Cwd, pre, {?NAMESPACE, ?PROVIDER}, Providers, State),

    [begin
         Opts = rebar_app_info:opts(AppInfo1),
         State1 = rebar_state:opts(State, Opts),
         {ok, PortSpecs} = pc_port_specs:construct(State1),
         ok = pc_compilation:compile_and_link(State1, PortSpecs)
     end || AppInfo1 <- Apps],

    rebar_hooks:run_all_hooks(
      Cwd, post, {?NAMESPACE, ?PROVIDER}, Providers, State),

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(no_app) ->
    io_lib:format("No so_name or application defined.", []);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
