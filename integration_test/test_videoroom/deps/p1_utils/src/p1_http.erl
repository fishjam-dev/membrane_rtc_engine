%%%-------------------------------------------------------------------
%%% File    : p1_http.erl
%%% Author  : Emilio Bustos <ebustos@process-one.net>
%%% Purpose : Provide a common API for inets / lhttpc / ibrowse
%%% Created : 29 Jul 2010 by Emilio Bustos <ebustos@process-one.net>
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

-module(p1_http).

-author('ebustos@process-one.net').

-export([start/0, stop/0, get/1, get/2, post/2, post/3,
	 request/3, request/4, request/5,
	 get_pool_size/0, set_pool_size/1]).

-type header() :: {string() | atom(), string()}.

-type headers() :: [header()].

-type option() :: {connect_timeout, timeout()} |
		  {timeout, timeout()} | {send_retry, non_neg_integer()} |
		  {partial_upload, non_neg_integer() | infinity} |
		  {partial_download, pid(), non_neg_integer() | infinity}.

-type options() :: [option()].

-type result() :: {ok,
		   {{pos_integer(), string()}, headers(), string()}} |
		  {error, atom()}.

-ifdef(USE_IBROWSE).

start() ->
    application:start(ibrowse).

stop() ->
    application:stop(ibrowse).

%% @spec (Method, URL, Hdrs, RequestBody, Options) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Options = [Option]
%%   Option = {timeout, Milliseconds | infinity} |
%%            {connect_timeout, Milliseconds | infinity} |
%%            {socket_options, [term()]}

%%   Milliseconds = integer()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string(), options()) -> result().
request(Method, URL, Hdrs, Body, Opts) ->
    TimeOut = proplists:get_value(timeout, Opts, infinity),
    Options = [{inactivity_timeout, TimeOut}
	       | proplists:delete(timeout, Opts)],
    case ibrowse:send_req(URL, Hdrs, Method, Body, Options)
	of
      {ok, Status, Headers, Response} ->
	  {ok, jlib:binary_to_integer(Status), Headers,
	   Response};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    application:get_env(ibrowse, default_max_sessions, 10).

set_pool_size(Size) ->
    application:set_env(ibrowse, default_max_sessions, Size).

-else.

-ifdef(USE_LHTTPC).

start() ->
    application:start(lhttpc).

stop() ->
    application:stop(lhttpc).

%% @spec (Method, URL, Hdrs, RequestBody, Options) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Options = [Option]
%%   Option = {timeout, Milliseconds | infinity} |
%%            {connect_timeout, Milliseconds | infinity} |
%%            {socket_options, [term()]}

%%   Milliseconds = integer()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string(), options()) -> result().
request(Method, URL, Hdrs, Body, Opts) ->
    {[TO, SO], Rest} = proplists:split(Opts, [timeout, socket_options]),
    TimeOut = proplists:get_value(timeout, TO, infinity),
    SockOpt = proplists:get_value(socket_options, SO, []),
    Options = [{connect_options, SockOpt} | Rest],
    Result = lhttpc:request(URL, Method, Hdrs, Body, TimeOut, Options),
    case Result of
      {ok, {{Status, _Reason}, Headers, Response}} ->
	  {ok, Status, Headers, (Response)};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    Opts = proplists:get_value(lhttpc_manager, lhttpc_manager:list_pools()),
    proplists:get_value(max_pool_size,Opts).

set_pool_size(Size) ->
    lhttpc_manager:set_max_pool_size(lhttpc_manager, Size).

-else.

start() ->
    application:start(inets).

stop() ->
    application:stop(inets).

to_list(Str) when is_binary(Str) ->
    binary_to_list(Str);
to_list(Str) ->
    Str.

%% @spec (Method, URL, Hdrs, RequestBody, Options) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Options = [Option]
%%   Option = {timeout, Milliseconds | infinity} |
%%            {connect_timeout, Milliseconds | infinity} |
%%            {socket_options, [term()]}

%%   Milliseconds = integer()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string(), options()) -> result().
request(Method, URLRaw, HdrsRaw, Body, Opts) ->
    Hdrs = lists:map(fun({N, V}) ->
                             {to_list(N), to_list(V)}
                     end, HdrsRaw),
    URL = to_list(URLRaw),

    Request = case Method of
		get -> {URL, Hdrs};
		head -> {URL, Hdrs};
		delete -> {URL, Hdrs};
		_ -> % post, etc.
		    {URL, Hdrs,
		     to_list(proplists:get_value(<<"content-type">>, HdrsRaw, [])),
                     Body}
	      end,
    Options = case proplists:get_value(timeout, Opts,
				       infinity)
		  of
		infinity -> proplists:delete(timeout, Opts);
		_ -> Opts
	      end,
    case httpc:request(Method, Request, Options, []) of
      {ok, {{_, Status, _}, Headers, Response}} ->
	  {ok, Status, Headers, Response};
      {error, Reason} -> {error, Reason}
    end.

get_pool_size() ->
    {ok, Size} = httpc:get_option(max_sessions),
    Size.

set_pool_size(Size) ->
    httpc:set_option(max_sessions, Size).

-endif.

-endif.

%% @spec (URL) -> Result
%%   URL = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a GET request.
%% Would be the same as calling `request(get, URL, [])',
%% that is {@link request/3} with an empty header list.
%% @end
%% @see request/3
-spec get(string()) -> result().
get(URL) -> request(get, URL, []).

%% @spec (URL, Hdrs) -> Result
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a GET request.
%% Would be the same as calling `request(get, URL, Hdrs)'.
%% @end
%% @see request/3
-spec get(string(), headers()) -> result().
get(URL, Hdrs) -> request(get, URL, Hdrs).

%% @spec (URL, RequestBody) -> Result
%%   URL = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a POST request with form data.
%% Would be the same as calling
%% `request(post, URL, [{"content-type", "x-www-form-urlencoded"}], Body)'.
%% @end
%% @see request/4
-spec post(string(), string()) -> result().
post(URL, Body) ->
    request(post, URL,
	    [{"content-type", "x-www-form-urlencoded"}],
	    Body).

%% @spec (URL, Hdrs, RequestBody) -> Result
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a POST request.
%% Would be the same as calling
%% `request(post, URL, Hdrs, Body)'.
%% @end
%% @see request/4
-spec post(string(), headers(), string()) -> result().
post(URL, Hdrs, Body) ->
    NewHdrs = case [X
		    || {X, _} <- Hdrs,
		       to_lower(X) == <<"content-type">>]
		  of
		[] ->
		    [{<<"content-type">>, <<"x-www-form-urlencoded">>}
		     | Hdrs];
		_ -> Hdrs
	      end,
    request(post, URL, NewHdrs, Body).

%% This function is copied from ejabberd's str.erl:
-spec to_lower(binary()) -> binary();
              (char()) -> char().

to_lower(B) when is_binary(B) ->
    iolist_to_binary(string:to_lower(binary_to_list(B)));
to_lower(C) ->
    string:to_lower(C).

%% @spec (Method, URL, Hdrs) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request without a body.
%% Would be the same as calling `request(Method, URL, Hdrs, [], [])',
%% that is {@link request/5} with an empty body.
%% @end
%% @see request/5
-spec request(atom(), string(), headers()) -> result().
request(Method, URL, Hdrs) ->
    request(Method, URL, Hdrs, [], []).

%% @spec (Method, URL, Hdrs, RequestBody) -> Result
%%   Method = atom()
%%   URL = string()
%%   Hdrs = [{Header, Value}]
%%   Header = string()
%%   Value = string()
%%   RequestBody = string()
%%   Result = {ok, StatusCode, Hdrs, ResponseBody}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ResponseBody = string()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(Method, URL, Hdrs, Body, [])', that is {@link request/5}
%% with no options.
%% @end
%% @see request/5
-spec request(atom(), string(), headers(), string()) -> result().
request(Method, URL, Hdrs, Body) ->
    request(Method, URL, Hdrs, Body, []).

% ibrowse {response_format, response_format()} |
% Options - [option()]
%                     Option - {sync, boolean()} | {stream, StreamTo} | {body_format, body_format()} | {full_result,
%                     boolean()} | {headers_as_is, boolean()}
%body_format() = string() | binary()
%                       The body_format option is only valid for the synchronous request and the default is  string.
%                     When making an asynchronous request the body will always be received as a binary.
% lhttpc: always binary

