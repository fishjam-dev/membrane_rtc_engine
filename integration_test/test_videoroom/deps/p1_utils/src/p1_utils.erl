%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017-2021 Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(p1_utils).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case p1_utils_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    case application:ensure_all_started(p1_utils) of
	{ok, _} -> ok;
	Err -> Err
    end.

stop() ->
    application:stop(p1_utils).
