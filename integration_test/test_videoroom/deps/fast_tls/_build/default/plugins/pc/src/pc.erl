-module(pc).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================

init(State) ->
    {ok, State1} = pc_prv_compile:init(State),
    {ok, State2} = pc_prv_clean:init(State1),
    {ok, State2}.

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
