%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% If your code need to be able to execute on ERTS versions both
%% earlier and later than 7.0, the best approach is to use the new
%% time API introduced in ERTS 7.0 and implement a fallback
%% solution using the old primitives to be used on old ERTS
%% versions. This way your code can automatically take advantage
%% of the improvements in the API when available. This is an
%% example of how to implement such an API, but it can be used
%% as is if you want to. Just add (a preferrably renamed version of)
%% this module to your project, and call the API via this module
%% instead of calling the BIFs directly.
%%

-module(p1_time_compat).

%% We don't want warnings about the use of erlang:now/0 in
%% this module.
-compile(nowarn_deprecated_function).
%%
%% We don't use
%%   -compile({nowarn_deprecated_function, [{erlang, now, 0}]}).
%% since this will produce warnings when compiled on systems
%% where it has not yet been deprecated.
%%

-export([monotonic_time/0,
	 monotonic_time/1,
	 system_time/0,
	 system_time/1,
	 os_system_time/0,
	 os_system_time/1,
	 time_offset/0,
	 time_offset/1,
	 convert_time_unit/3,
	 timestamp/0,
	 unique_timestamp/0,
	 unique_integer/0,
	 unique_integer/1,
	 monitor/2,
	 system_info/1,
	 system_flag/2]).

-ifdef(NEED_TIME_FALLBACKS).
monotonic_time() ->
    erlang_system_time_fallback().

monotonic_time(Unit) ->
    STime = erlang_system_time_fallback(),
    convert_time_unit_fallback(STime, native, Unit).

system_time() ->
    erlang_system_time_fallback().

system_time(Unit) ->
    STime = erlang_system_time_fallback(),
    convert_time_unit_fallback(STime, native, Unit).

os_system_time() ->
    os_system_time_fallback().

os_system_time(Unit) ->
    STime = os_system_time_fallback(),
    try
        convert_time_unit_fallback(STime, native, Unit)
    catch
        error:bad_time_unit -> erlang:error(badarg, [Unit])
    end.

time_offset() ->
    %% Erlang system time and Erlang monotonic
    %% time are always aligned
    0.

time_offset(Unit) ->
    _ = integer_time_unit(Unit),
    %% Erlang system time and Erlang monotonic
    %% time are always aligned
    0.

convert_time_unit(Time, FromUnit, ToUnit) ->
    try
        convert_time_unit_fallback(Time, FromUnit, ToUnit)
    catch
        _:_ ->
            erlang:error(badarg, [Time, FromUnit, ToUnit])
    end.

timestamp() ->
    erlang:now().

unique_timestamp() ->
    erlang:now().

unique_integer() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

unique_integer(Modifiers) ->
    case is_valid_modifier_list(Modifiers) of
        true ->
            %% now() converted to an integer
            %% fullfill the requirements of
            %% all modifiers: unique, positive,
            %% and monotonic...
            {MS, S, US} = erlang:now(),
            (MS*1000000+S)*1000000+US;
        false ->
            erlang:error(badarg, [Modifiers])
    end.

monitor(Type, Item) ->
    try
	erlang:monitor(Type, Item)
    catch
	error:Error ->
	    case {Error, Type, Item} of
		{badarg, time_offset, clock_service} ->
		    %% Time offset is final and will never change.
		    %% Return a dummy reference, there will never
		    %% be any need for 'CHANGE' messages...
		    make_ref();
		_ ->
		    erlang:error(Error, [Type, Item])
	    end
    end.

system_info(Item) ->
    try
	erlang:system_info(Item)
    catch
	error:badarg ->
	    case Item of
		time_correction ->
		    case erlang:system_info(tolerant_timeofday) of
			enabled -> true;
			disabled -> false
		    end;
		time_warp_mode ->
		    no_time_warp;
		time_offset ->
		    final;
		NotSupArg when NotSupArg == os_monotonic_time_source;
			       NotSupArg == os_system_time_source;
			       NotSupArg == start_time;
			       NotSupArg == end_time ->
		    %% Cannot emulate this...
		    erlang:error(notsup, [NotSupArg]);
		_ ->
		    erlang:error(badarg, [Item])
	    end;
	error:Error ->
	    erlang:error(Error, [Item])
    end.

system_flag(Flag, Value) ->
    try
	erlang:system_flag(Flag, Value)
    catch
	error:Error ->
	    case {Error, Flag, Value} of
		{badarg, time_offset, finalize} ->
		    %% Time offset is final
		    final;
		_ ->
		    erlang:error(Error, [Flag, Value])
	    end
    end.

%%
%% Internal functions
%%

integer_time_unit(native) -> 1000*1000;
integer_time_unit(nano_seconds) -> 1000*1000*1000;
integer_time_unit(micro_seconds) -> 1000*1000;
integer_time_unit(milli_seconds) -> 1000;
integer_time_unit(seconds) -> 1;
integer_time_unit(I) when is_integer(I), I > 0 -> I;
integer_time_unit(BadRes) -> erlang:error(badarg, [BadRes]).

erlang_system_time_fallback() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

os_system_time_fallback() ->
    {MS, S, US} = os:timestamp(),
    (MS*1000000+S)*1000000+US.

convert_time_unit_fallback(Time, FromUnit, ToUnit) ->
    FU = integer_time_unit(FromUnit),
    TU = integer_time_unit(ToUnit),
    case Time < 0 of
	true -> TU*Time - (FU - 1);
	false -> TU*Time
    end div FU.

is_valid_modifier_list([positive|Ms]) ->
    is_valid_modifier_list(Ms);
is_valid_modifier_list([monotonic|Ms]) ->
    is_valid_modifier_list(Ms);
is_valid_modifier_list([]) ->
    true;
is_valid_modifier_list(_) ->
    false.
-else.
monotonic_time() ->
    erlang:monotonic_time().

monotonic_time(Unit) ->
    erlang:monotonic_time(Unit).

system_time() ->
    erlang:system_time().

system_time(Unit) ->
    erlang:system_time(Unit).

os_system_time() ->
    os:system_time().

os_system_time(Unit) ->
    os:system_time(Unit).

time_offset() ->
    erlang:time_offset().

time_offset(Unit) ->
    erlang:time_offset(Unit).

convert_time_unit(Time, FromUnit, ToUnit) ->
    erlang:convert_time_unit(Time, FromUnit, ToUnit).

timestamp() ->
    erlang:timestamp().

unique_timestamp() ->
    {MS, S, _} = erlang:timestamp(),
    {MS, S, erlang:unique_integer([positive, monotonic])}.

unique_integer() ->
    erlang:unique_integer().

unique_integer(Modifiers) ->
    erlang:unique_integer(Modifiers).

monitor(Type, Item) ->
    erlang:monitor(Type, Item).

system_info(Item) ->
    erlang:system_info(Item).

system_flag(Flag, Value) ->
    erlang:system_flag(Flag, Value).

-endif.
