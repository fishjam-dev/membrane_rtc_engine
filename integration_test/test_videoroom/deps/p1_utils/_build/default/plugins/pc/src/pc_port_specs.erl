%% -------------------------------------------------------------------
%%
%% This file contains substantial portions of the original rebar_port_compiler.
%% Special thanks to all the folks that contributed to that effort.
%%
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% -------------------------------------------------------------------
-module(pc_port_specs).

-export([
         construct/1,
         create_env/2,
         %% spec accessors
         environment/1,
         opts/1,
         objects/1,
         sources/1,
         target/1,
         type/1,
         link_lang/1,
         object_file_ext/0
        ]).
-export_type([spec/0]).

-record(spec, {type::'drv' | 'exe',
               link_lang::'cc' | 'cxx',
               target::file:filename(),
               sources = [] :: [file:filename(), ...],
               objects = [] :: [file:filename(), ...],
               opts = [] :: list() | []}).

-opaque spec() :: #spec{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec construct(rebar_state:t()) -> {ok, [spec()]} |
                                    {error, Reason :: any()}.
construct(State) ->
    case rebar_state:get(State, port_specs, []) of
        [] ->
            {ok, [port_spec_from_legacy(State)]};
        PortSpecs ->
            Filtered = lists:filter(fun filter_port_spec/1, PortSpecs),
            Specs = [get_port_spec(State, os:type(), Spec) || Spec <- Filtered],
            {ok, [S || S <- Specs, S#spec.sources /= []]}
    end.

create_env(State, Spec) ->
    pc_port_specs:environment(Spec) ++
        try_and_create_env(State).

%% == Spec Accessors ==

environment(#spec{opts = Opts})        -> proplists:get_value(env, Opts).
opts(#spec{opts = Opts})               -> Opts.
objects(#spec{objects = Objects})      -> Objects.
sources(#spec{sources = Sources})      -> Sources.
target(#spec{target = Target})         -> Target.
type(#spec{type = Type})               -> Type.
link_lang(#spec{link_lang = LinkLang}) -> LinkLang.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

try_and_create_env(State) ->
    _ = code:ensure_loaded(rebar_env),
    case erlang:function_exported(rebar_env, create_env, 1) of
        false -> [];
        true -> rebar_env:create_env(State)
    end.

port_spec_from_legacy(Config) ->
    %% Get the target from the so_name variable
    OutDir = case os:getenv("REBAR_BARE_COMPILER_OUTPUT_DIR", undefined) of
                 undefined ->
                     "priv";
                 Dir ->
                     filename:join(Dir, "priv")
             end,
    Target = case rebar_state:get(Config, so_name, undefined) of
                 undefined ->
                     %% Generate a sensible default from app file
                     AppName = case rebar_state:current_app(Config) of
                                      undefined ->
                                          throw({error, {pc_prv_compile, no_app}});
                                      CurrentApp ->
                                          BAppName = rebar_app_info:name(CurrentApp),
                                          binary_to_list(BAppName)
                                  end,
                     filename:join(OutDir, lists:concat([AppName, "_drv.so"]));
                 AName ->
                     filename:join(OutDir, AName)
             end,

    %% Get the list of source files from port_sources
    Sources = port_sources(rebar_state:dir(Config), rebar_state:get(Config, port_sources, ["c_src/*.c"])),
    #spec { type = pc_util:target_type(Target),
            link_lang = cc,
            target = maybe_switch_extension(os:type(), Target),
            sources = Sources,
            objects = port_objects(Sources),
            opts    = [port_opt(Config, O) || O <- fill_in_defaults([])]}.

maybe_report_wc(Source, Found) ->
    case Found of
        [] ->
            rebar_api:warn("Found no source files for:~n~p", [Source]);
        _ ->
            ok
    end,
    Found.

wildcard(Source, WorkDir) ->
    maybe_report_wc(Source, filelib:wildcard(Source, WorkDir)).

port_sources(WorkDir, Sources) ->
    lists:flatmap(fun (Source) ->
                      wildcard(Source, WorkDir)
                  end, Sources).

maybe_switch_extension({win32, nt}, Target) ->
    switch_to_dll_or_exe(Target);
maybe_switch_extension(_OsType, Target) ->
    Target.

port_objects(SourceFiles) ->
    Ext = object_file_ext(),
    [pc_util:replace_extension(O, Ext) || O <- SourceFiles].

filter_port_spec({ArchRegex, _, _, _}) ->
    pc_util:is_arch(ArchRegex);
filter_port_spec({ArchRegex, _, _}) ->
    pc_util:is_arch(ArchRegex);
filter_port_spec({_, _}) ->
    true.

get_port_spec(Config, OsType, {Target, Sources}) ->
    get_port_spec(Config, OsType, {undefined, Target, Sources, []});
get_port_spec(Config, OsType, {Arch, Target, Sources}) ->
    get_port_spec(Config, OsType, {Arch, Target, Sources, []});
get_port_spec(Config, OsType, {_Arch, Target, Sources, Opts}) ->
    Env = try_and_create_env(Config),
    ProjectRoot = rebar_state:dir(Config),
    SourceFiles =
        lists:flatmap(
          fun(Source) ->
                  wildcard(expand_env(Source, Env), rebar_state:dir(Config))
          end, Sources),
    LinkLang =
        case lists:any(
               fun(Src) ->
                       pc_compilation:compiler(filename:extension(Src)) == "$CXX"
               end,
               SourceFiles)
        of
            true  -> cxx;
            false -> cc
        end,
   
    Target1 =
        case os:getenv("REBAR_BARE_COMPILER_OUTPUT_DIR", undefined) of
            undefined ->
                filename:join(ProjectRoot, Target);
            Dir -> 
                filename:join([Dir, Target])
        end,
    #spec{type      = pc_util:target_type(Target1),
          target    = coerce_extension(OsType, Target1),
          link_lang = LinkLang,
          sources   = SourceFiles,
          objects   = port_objects(SourceFiles),
          opts      = [port_opt(Config, O) || O <- fill_in_defaults(Opts)]}.

expand_env(Source, Env) ->
    case pc_util:strchr(Source, $$) of
        0 ->
            %% No variables to expand. Also hides undef on older rebar3.
            Source;
        _ ->
            lists:foldl(
              fun({Key, Value}, Acc) ->
                      %% TODO: the expand_env_variable/3 only expands
                      %% variables delimited by whitespace and inside
                      %% ${}. Either fix or add a new function to
                      %% rebar3 or make a new function here in pc.
                      rebar_utils:expand_env_variable(Acc, Key, Value)
              end, Source, Env)
    end.

coerce_extension({win32, nt}, Target) ->
    switch_to_dll_or_exe(Target);
coerce_extension(_OsType, Target) ->
    Target.

switch_to_dll_or_exe(Target) ->
    case filename:extension(Target) of
        ".so"  -> filename:rootname(Target, ".so") ++ ".dll";
        []     -> Target ++ ".exe";
        _Other -> Target
    end.

%% NOTE: By ensuring that the options list has default values we can simplify
%% the implementation of the compilation module. More invariants means less
%% lookups and/or branching.
%%
%% It might make sense to move 'env' into the top-level of the record. That's an
%% option too.
fill_in_defaults(Opts) ->
    %% insert an {env, []} if {env, _} does not exist
    case lists:any(fun ({env, _}) -> true;
                       (_)        -> false end, Opts) of
        true ->
            Opts;
        false ->
            [{env, []} | Opts]
    end.

port_opt(State, {env, Env}) ->
    {ok, PortEnv} = pc_port_env:construct(State, Env),
    {env, PortEnv};
port_opt(_State, Opt) ->
    Opt.

object_file_ext() ->
    case os:type() of
        {win32,_} -> ".obj";
        _         -> ".o"
    end.
