%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
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
-module(p1_options).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, insert/4, delete/3, lookup/3, clear/1,
	 compile/1]).
%% For debug only
-export([dump/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type scope() :: global | any().
-record(state, {tab :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(atom()) -> ok | {error, already_started | any()}.
start(Tab) ->
    case whereis(Tab) of
	undefined ->
	    application:ensure_all_started(p1_utils),
	    Spec = {?MODULE, {?MODULE, start_link, [Tab]},
		    permanent, 5000, worker, [?MODULE]},
	    case supervisor:start_child(p1_utils_sup, Spec) of
		{ok, _} -> ok;
		{error, {already_started, _}} -> {error, already_started};
		{error, _} = Err -> Err
	    end;
	_ ->
	    {error, already_started}
    end.

-spec start_link(atom()) -> {ok, pid()} | {error, any()}.
start_link(Tab) ->
    gen_server:start_link({local, Tab}, ?MODULE, [Tab], []).

-spec insert(atom(), atom(), scope(), any()) -> ok.
insert(Tab, Opt, Scope, Val) ->
    ets:insert(Tab, {{Opt, Scope}, Val}),
    ok.

-spec delete(atom(), atom(), scope()) -> ok.
delete(Tab, Opt, Scope) ->
    ets:delete(Tab, {Opt, Scope}),
    ok.

-spec lookup(atom(), atom(), scope()) -> {ok, any()} | undefined.
lookup(Tab, Opt, Scope) ->
    case ets:lookup(Tab, {Opt, Scope}) of
	[] -> undefined;
	[{_, Val}] -> {ok, Val}
    end.

-spec clear(atom()) -> ok.
clear(Tab) ->
    ets:delete_all_objects(Tab),
    ok.

-spec compile(atom()) -> ok.
compile(Tab) ->
    case gen_server:call(Tab, compile, timer:minutes(1)) of
	ok -> ok;
	{error, Reason} ->
	    error_logger:error_msg(
	      "Failed to compile configuration for ~p: ~s",
	      [Tab, format_error(Reason)]),
	    erlang:error({compile_failed, Tab})
    end.

-spec dump(atom()) -> ok.
dump(Mod) ->
    Exprs = get_exprs(Mod),
    File = filename:join("/tmp", atom_to_list(Mod) ++ ".erl"),
    case file:write_file(File, string:join(Exprs, io_lib:nl())) of
	ok ->
	    %% erl_tidy:file(File, [{backups, false}]),
	    io:format("Dynamic module '~s' is written to ~ts~n", [Mod, File]);
	{error, Reason} ->
	    io:format("Failed to dump dynamic module '~s' to ~ts: ~s~n",
		      [Mod, File, file:format_error(Reason)])
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Tab]) ->
    catch ets:new(Tab, [named_table, public, {read_concurrency, true}]),
    {ok, #state{tab = Tab}}.

handle_call(compile, From, #state{tab = Tab} = State) ->
    do_compile(Tab, [From]),
    {noreply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec do_compile(atom(), list()) -> ok.
do_compile(Tab, Callers) ->
    receive
	{'gen_call', Caller, compile} ->
	    do_compile(Tab, [Caller|Callers])
    after 0 ->
	    Exprs = get_exprs(Tab),
	    Result = compile_exprs(Tab, Exprs),
	    lists:foreach(
	      fun(Caller) ->
		      gen_server:reply(Caller, Result)
	      end, lists:reverse(Callers))
    end.

-spec get_exprs(atom()) -> [string()].
get_exprs(Mod) ->
    OptMap = ets:foldl(
	       fun({{Opt, Scope}, Val}, Acc) ->
		       Vals = maps:get(Opt, Acc, []),
		       maps:put(Opt, [{Scope, Val}|Vals], Acc)
	       end, #{}, Mod),
    Opts = maps:fold(
	     fun(Opt, Vals, Acc) ->
		     Default = case lists:keyfind(global, 1, Vals) of
				   {_, V} -> {ok, V};
				   false -> undefined
			       end,
		     [lists:flatmap(
			fun({Scope, Val}) when {ok, Val} /= Default ->
				io_lib:format(
				  "~p(~p) -> {ok, ~p};~n", [Opt, Scope, Val]);
			   (_) ->
				""
			end, Vals) ++ io_lib:format("~p(_) -> ~p.", [Opt, Default])
		      |Acc]
	     end, [], OptMap),
    Known = maps:fold(
	      fun(Opt, _, Acc) ->
		      io_lib:format(
			"is_known(~p) -> true;~n", [Opt]) ++ Acc
	      end, "", OptMap) ++ "is_known(_) -> false.",
    Scopes = maps:fold(
	       fun(Opt, Vals, Acc) ->
		       io_lib:format(
			 "get_scope(~p) -> ~p;~n",
			 [Opt, [Scope || {Scope, _} <- Vals]]) ++ Acc
	       end, "", OptMap) ++ "get_scope(_) -> [].",
    [io_lib:format("-module(~p).", [Mod]),
     "-compile(export_all).",
     Known, Scopes | Opts].

-spec compile_exprs(module(), [string()]) -> ok | {error, any()}.
compile_exprs(Mod, Exprs) ->
    try
	Forms = lists:map(
		  fun(Expr) ->
			  {ok, Tokens, _} = erl_scan:string(lists:flatten(Expr)),
			  {ok, Form} = erl_parse:parse_form(Tokens),
			  Form
		  end, Exprs),
	{ok, Code} = case compile:forms(Forms, []) of
			 {ok, Mod, Bin} -> {ok, Bin};
			 {ok, Mod, Bin, _Warnings} -> {ok, Bin};
			 Error -> Error
		     end,
	{module, Mod} = code:load_binary(Mod, "nofile", Code),
	ok
    catch _:{badmatch, {error, ErrInfo, _ErrLocation}} ->
	    {error, ErrInfo};
	  _:{badmatch, {error, _} = Err} ->
	    Err;
	  _:{badmatch, error} ->
	    {error, compile_failed}
    end.

format_error({_Line, _Mod, _Term} = Reason) ->
    "Syntax error at line " ++ file:format_error(Reason);
format_error(Reason) ->
    atom_to_list(Reason).
