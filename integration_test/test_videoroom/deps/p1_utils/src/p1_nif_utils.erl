%%%-------------------------------------------------------------------
%%% File    : p1_nif_utils.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Description : Helper utilities for handling nif code
%%%
%%% Created : 7 Oct 2015 by Paweł Chmielowski <pawel@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2021 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(p1_nif_utils).

-export([get_so_path/3]).

get_so_path(ModuleName, AppNames, SoName) ->
    PrivDir = first_match(fun(App) ->
                                  case code:priv_dir(App) of
                                      {error, _} -> none;
                                      V -> V
                                  end
                          end, AppNames),
    case PrivDir of
        none ->
            Ext = case os:type() of
                      {win32, _} -> ".dll";
                      _ -> ".so"
                  end,
            SoFName = filename:join(["priv", "lib", SoName ++ Ext]),
            LPath = first_match(fun(Path) ->
                                        P = case filename:basename(Path) of
                                                "ebin" -> filename:dirname(Path);
                                                _ -> Path
                                            end,
                                        case filelib:is_file(filename:join([P, SoFName])) of
                                            true ->
                                                filename:join([P, "priv", "lib", SoName]);
                                    _ ->
                                                none
                                        end
                                end, code:get_path()),
            case LPath of
                none ->
                    EbinDir = filename:dirname(code:which(ModuleName)),
                    AppDir = filename:dirname(EbinDir),
                    filename:join([AppDir, "priv", "lib", SoName]);
                Val ->
                    Val
            end;
        V ->
            filename:join([V, "lib", SoName])
    end.

first_match(_Fun, []) ->
    none;
first_match(Fun, [H|T]) ->
    case Fun(H) of
        none ->
            first_match(Fun, T);
        V  ->
            V
    end.
