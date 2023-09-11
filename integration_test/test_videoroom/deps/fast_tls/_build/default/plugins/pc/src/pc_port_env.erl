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
-module(pc_port_env).

-export([construct/1, construct/2]).
-export_type([]).

%%%===================================================================
%%% API
%%%===================================================================

construct(State) ->
    construct(State, []).

-spec construct(rebar_state:t()) -> {ok, [pc_port_specs:spec()]} |
                                    {error, Reason :: any()}.
construct(State, ExtraEnv) ->
    DefaultEnv  = filter_env(default_env()),
    PortEnv     = filter_env(rebar_state:get(State, port_env, [])),
    OverrideEnv = PortEnv ++ ExtraEnv,

    RawEnv = apply_defaults(os_env(), DefaultEnv) ++ OverrideEnv,
    {ok, expand_vars_loop(merge_each_var(RawEnv))}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%
%% Given a list of {Key, Value} environment variables, where Key may be defined
%% multiple times, walk the list and expand each self-reference so that we
%% end with a list of each variable singly-defined.
%%
merge_each_var(Vars) ->
    lists:foldl(fun ({Key, Value}, Acc) ->
                        Evalue = case orddict:find(Key, Acc) of
                                     error ->
                                         %% Nothing yet defined for this key/value.
                                         %% Expand any self-references as blank.
                                         rebar_api:expand_env_variable(Value, Key, "");
                                     {ok, Value0} ->
                                         %% Use previous definition in expansion
                                         rebar_api:expand_env_variable(Value, Key, Value0)
                                 end,
                        orddict:store(Key, Evalue, Acc)
                end, [], Vars).

%%
%% Give a unique list of {Key, Value} environment variables, expand each one
%% for every other key until no further expansions are possible.
%%
expand_vars_loop(Vars) ->
    expand_vars_loop(Vars, [], dict:from_list(Vars), 10).

expand_vars_loop(_Pending, _Recurse, _Vars, 0) ->
    rebar_api:abort("Max. expansion reached for ENV vars!\n", []);
expand_vars_loop([], [], Vars, _Count) ->
    lists:keysort(1, dict:to_list(Vars));
expand_vars_loop([], Recurse, Vars, Count) ->
    expand_vars_loop(Recurse, [], Vars, Count-1);
expand_vars_loop([{K, V} | Rest], Recurse, Vars, Count) ->
    %% Identify the variables that need expansion in this value
    ReOpts = [global, {capture, all_but_first, list}, unicode],
    case re:run(V, "\\\${?(\\w+)}?", ReOpts) of
        {match, Matches} ->
            %% Identify the unique variables that need to be expanded
            UniqueMatches = lists:usort([M || [M] <- Matches]),

            %% For each variable, expand it and return the final
            %% value. Note that if we have a bunch of unresolvable
            %% variables, nothing happens and we don't bother
            %% attempting further expansion
            case expand_keys_in_value(UniqueMatches, V, Vars) of
                V ->
                    %% No change after expansion; move along
                    expand_vars_loop(Rest, Recurse, Vars, Count);
                Expanded ->
                    %% Some expansion occurred; move to next k/v but
                    %% revisit this value in the next loop to check
                    %% for further expansion
                    NewVars = dict:store(K, Expanded, Vars),
                    expand_vars_loop(Rest, [{K, Expanded} | Recurse],
                                     NewVars, Count)
            end;

        nomatch ->
            %% No values in this variable need expansion; move along
            expand_vars_loop(Rest, Recurse, Vars, Count)
    end.

expand_keys_in_value([], Value, _Vars) ->
    Value;
expand_keys_in_value([Key | Rest], Value, Vars) ->
    NewValue = case dict:find(Key, Vars) of
                   {ok, KValue} ->
                       rebar_api:expand_env_variable(Value, Key, KValue);
                   error ->
                       Value
               end,
    expand_keys_in_value(Rest, NewValue, Vars).

%%
%% Filter a list of env vars such that only those which match the provided
%% architecture regex (or do not have a regex) are returned.
%%
filter_env(Env) ->
    Res = lists:foldl(fun
                          ({ArchRegex, Key, Value}, Acc) ->
                             case pc_util:is_arch(ArchRegex) of
                                 true -> [{Key,Value} | Acc];
                                 false -> Acc
                             end;
                          ({Key,Value}, Acc) ->
                             [{Key,Value} | Acc]
                     end, [], Env),
    lists:reverse(Res).

%%
%% Given a list of {Key, Value} variables, and another list of default
%% {Key, Value} variables, return a merged list where the rule is if the
%% default is expandable expand it with the value of the variable list,
%% otherwise just return the value of the variable.
%%
apply_defaults(Vars, Defaults) ->
    dict:to_list(
      dict:merge(fun(Key, VarValue, DefaultValue) ->
                         case is_expandable(DefaultValue) of
                             true ->
                                 rebar_api:expand_env_variable(DefaultValue,
                                                               Key,
                                                               VarValue);
                             false -> VarValue
                         end
                 end,
                 dict:from_list(Vars),
                 dict:from_list(Defaults))).

os_env() ->
    ReOpts = [{return, list}, {parts, 2}, unicode],
    Os = [list_to_tuple(re:split(S, "=", ReOpts)) ||
             S <- lists:filter(fun discard_deps_vars/1, os:getenv())],
    %% Drop variables without a name (win32)
    [T1 || {K, _V} = T1 <- Os, K =/= []].

%%
%% To avoid having multiple repetitions of the same environment variables
%% (ERL_LIBS), avoid exporting any variables that may cause conflict with
%% those exported by the rebar_deps module (ERL_LIBS, REBAR_DEPS_DIR)
%%
discard_deps_vars("ERL_LIBS=" ++ _Value)       -> false;
discard_deps_vars("REBAR_DEPS_DIR=" ++ _Value) -> false;
discard_deps_vars(_Var)                        -> true.

%%
%% Given a string, determine if it is expandable
%%
is_expandable(InStr) ->
    case re:run(InStr,"\\\$",[{capture,none}]) of
        match -> true;
        nomatch -> false
    end.

erl_interface_dir(Subdir) ->
    case code:lib_dir(erl_interface, Subdir) of
        {error, bad_name} ->
            throw({error, {erl_interface,Subdir,"code:lib_dir(erl_interface)"
                           "is unable to find the erl_interface library."}});
        Dir -> Dir
    end.

erts_dir() ->
    lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

default_env() ->
    ErlInterfaceIncludeDir = erl_interface_dir(include),
    ErlInterfaceHeader = filename:join(ErlInterfaceIncludeDir, "erl_interface.h"),
    ErlInterfaceLibs = case filelib:is_file(ErlInterfaceHeader) of
                           true -> ["erl_interface", "ei"];
                           false -> ["ei"]
                       end,
    Arch = os:getenv("REBAR_TARGET_ARCH"),
    Vsn = os:getenv("REBAR_TARGET_ARCH_VSN"),
    [
     {"CC", get_tool(Arch, Vsn, "gcc", "cc")},
     {"CXX", get_tool(Arch, Vsn, "g++", "c++")},
     {"AR", get_tool(Arch, "ar", "ar")},
     {"AS", get_tool(Arch, "as", "as")},
     {"CPP", get_tool(Arch, Vsn, "cpp", "cpp")},
     {"LD", get_tool(Arch, "ld", "ld")},
     {"RANLIB", get_tool(Arch, Vsn, "ranlib", "ranlib")},
     {"STRIP", get_tool(Arch, "strip", "strip")},
     {"NM", get_tool(Arch, "nm", "nm")},
     {"OBJCOPY", get_tool(Arch, "objcopy", "objcopy")},
     {"OBJDUMP", get_tool(Arch, "objdump", "objdump")},

     {"DRV_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_CC_TEMPLATE",
      "$CC -c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $LDLIBS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"DRV_LINK_CXX_TEMPLATE",
      "$CXX $PORT_IN_FILES $LDFLAGS $LDLIBS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"EXE_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_CC_TEMPLATE",
      "$CC -c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
     {"EXE_LINK_CXX_TEMPLATE",
      "$CXX $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
     {"DRV_CFLAGS" , "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
     {"DRV_LDFLAGS", "-shared $ERL_LDFLAGS"},
     {"EXE_CFLAGS" , "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
     {"EXE_LDFLAGS", "$ERL_LDFLAGS"},

     {"ERL_CFLAGS", lists:concat(
                      [
                       " -I\"", ErlInterfaceIncludeDir,
                       "\" -I\"", filename:join(erts_dir(), "include"),
                       "\" "
                      ])},
     {"ERL_EI_LIBDIR", lists:concat(["\"", erl_interface_dir(lib), "\""])},
     {"ERL_LDFLAGS"  , lists:concat([" -L$ERL_EI_LIBDIR"] ++
                                    [" -l"++EiLib || EiLib <- ErlInterfaceLibs])},
     {"ERLANG_ARCH"  , rebar_api:wordsize()},
     {"ERLANG_TARGET", rebar_api:get_arch()},

     {"darwin", "DRV_LDFLAGS",
      "-bundle -flat_namespace -undefined suppress $ERL_LDFLAGS"},

     %% Solaris specific flags
     {"solaris.*-64$", "CFLAGS", "-D_REENTRANT -m64 $CFLAGS"},
     {"solaris.*-64$", "CXXFLAGS", "-D_REENTRANT -m64 $CXXFLAGS"},
     {"solaris.*-64$", "LDFLAGS", "-m64 $LDFLAGS"},

     %% OS X Leopard flags for 64-bit
     {"darwin9.*-64$", "CFLAGS", "-m64 $CFLAGS"},
     {"darwin9.*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
     {"darwin9.*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

     %% OS X Lion onwards flags for 64-bit
     {"darwin1[0-4].*-64$", "CFLAGS", "-m64 $CFLAGS"},
     {"darwin1[0-4].*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
     {"darwin1[0-4].*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

     %% OS X Snow Leopard, Lion, and Mountain Lion flags for 32-bit
     {"darwin1[0-2].*-32", "CFLAGS", "-m32 $CFLAGS"},
     {"darwin1[0-2].*-32", "CXXFLAGS", "-m32 $CXXFLAGS"},
     {"darwin1[0-2].*-32", "LDFLAGS", "-arch i386 -flat_namespace -undefined suppress $LDFLAGS"},

     %% Windows specific flags
     %% add MS Visual C++ support to rebar on Windows
     {"win32", "CC", "cl.exe"},
     {"win32", "CXX", "cl.exe"},
     {"win32", "LINKER", "link.exe"},
     {"win32", "DRV_CXX_TEMPLATE",
      %% DRV_* and EXE_* Templates are identical
      "$CXX /c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_CC_TEMPLATE",
      "$CC /c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
     {"win32", "DRV_LINK_CXX_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% DRV_* and EXE_* Templates are identical
     {"win32", "EXE_CXX_TEMPLATE",
      "$CXX /c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_CC_TEMPLATE",
      "$CC /c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
     {"win32", "EXE_LINK_CXX_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% ERL_CFLAGS are ok as -I even though strictly it should be /I
     {"win32", "ERL_LDFLAGS",
      lists:concat([" /LIBPATH:$ERL_EI_LIBDIR "] ++
                    [EiLib++".lib " || EiLib <- ErlInterfaceLibs])},
     {"win32", "DRV_CFLAGS", "/Zi /Wall $ERL_CFLAGS"},
     {"win32", "DRV_LDFLAGS", "/DLL $ERL_LDFLAGS"}
    ].

get_tool(Arch, Tool, Default) ->
    get_tool(Arch, false, Tool, Default).

get_tool(false, _, _, Default) -> Default;
get_tool("", _, _, Default) -> Default;
get_tool(Arch, false, Tool, _Default) -> Arch++"-"++Tool;
get_tool(Arch, "", Tool, _Default) -> Arch++"-"++Tool;
get_tool(Arch, Vsn, Tool, _Default) -> Arch++"-"++Tool++"-"++Vsn.
