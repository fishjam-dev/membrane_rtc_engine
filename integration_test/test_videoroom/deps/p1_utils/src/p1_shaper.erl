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
-module(p1_shaper).

%% API
-export([new/1, new/2, update/2, pp/1]).

-record(state, {maxrate = 0 :: integer(),
		burst_size = 0 :: integer(),
		acquired_credit = 0 :: integer(),
		lasttime = 0 :: integer()}).

-opaque state() :: #state{}.
-export_type([state/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(integer()) -> state().
new(MaxRate) ->
    new(MaxRate, MaxRate).

-spec new(integer(), integer()) -> state().
new(MaxRate, BurstSize) ->
    #state{maxrate = MaxRate,
	   burst_size = BurstSize,
	   acquired_credit = BurstSize,
	   lasttime = p1_time_compat:system_time(micro_seconds)}.

-spec update(state(), non_neg_integer()) -> {state(), non_neg_integer()}.
update(#state{maxrate = MR, burst_size = BS,
	      acquired_credit = AC, lasttime = L} = State, Size) ->
    Now = p1_time_compat:system_time(micro_seconds),
    AC2 = min(BS, AC + (MR*(Now - L) div 1000000) - Size),
    Pause = if AC2 >= 0 -> 0;
		true -> -1000*AC2 div MR
	    end,
    {State#state{acquired_credit = AC2, lasttime = Now}, Pause}.

-spec pp(any()) -> iolist().
pp(Term) ->
    io_lib_pretty:print(Term, fun pp/2).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec pp(atom(), non_neg_integer()) -> [atom()] | no.
pp(state, 4) -> record_info(fields, state);
pp(_, _) -> no.
