%%%----------------------------------------------------------------------
%%% File    : stun_listener.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose :
%%% Created : 9 Jan 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
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
%%%----------------------------------------------------------------------

-module(stun_listener).

-behaviour(gen_server).

%% API
-export([start_link/0, add_listener/5, del_listener/3, start_listener/6]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("stun_logger.hrl").

-define(TCP_SEND_TIMEOUT, 10000).
-define(UDP_READ_PACKETS, 100).
-define(UDP_RECBUF, 1024 * 1024). % 1 MiB

-record(state, {listeners = #{}}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_listener(IP, MinPort, MaxPort, Transport, Opts) ->
    gen_server:call(?MODULE, {add_listener, IP, MinPort, MaxPort, Transport, Opts}).

del_listener(IP, Port, Transport) ->
    gen_server:call(?MODULE, {del_listener, IP, Port, Transport}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({add_listener, IP, Port, {MinPort, MaxPort}, Transport, Opts},
            _From,
            State) ->
    {Pid, MRef} =
        spawn_monitor(?MODULE,
                      start_listener,
                      [IP, Port, {MinPort, MaxPort}, Transport, Opts, self()]),
    receive
        {'DOWN', MRef, _Type, _Object, Info} ->
            Res = {error, Info},
            format_listener_error(IP, MinPort, MaxPort, Transport, Opts, Res),
            {reply, Res, State};
        {Pid, Reply} ->
            case Reply of
                {error, _} = Err ->
                    format_listener_error(IP, MinPort, MaxPort, Transport, Opts, Err),
                    {reply, Reply, State};
                {ok, UsedPort} ->
                    Listeners =
                        maps:put({IP, UsedPort, Transport},
                                 {MRef, Pid, Opts},
                                 State#state.listeners),
                    {reply, {ok, UsedPort, Pid}, State#state{listeners = Listeners}}
            end
    end;
handle_call({del_listener, IP, Port, Transport}, _From, State) ->
    case maps:find({IP, Port, Transport}, State#state.listeners) of
        {ok, {MRef, Pid, _Opts}} ->
            catch erlang:demonitor(MRef, [flush]),
            catch exit(Pid, kill),
            Listeners = maps:remove({IP, Port, Transport}, State#state.listeners),
            {reply, ok, State#state{listeners = Listeners}};
        error ->
            {reply, {error, notfound}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, _Type, _Pid, Info}, State) ->
    Listeners =
        maps:filter(fun ({IP, Port, Transport}, {Ref, _, _}) when Ref == MRef ->
                            ?LOG_ERROR("Listener on ~s (~s) failed: ~p",
                                       [stun_logger:encode_addr({IP, Port}), Transport, Info]),
                            false;
                        (_, _) ->
                            true
                    end,
                    State#state.listeners),
    {noreply, State#state{listeners = Listeners}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_listener(IP, ClientPort, {MinPort, MaxPort}, Transport, Opts, Owner)
    when Transport == tcp; Transport == tls ->
    OpenFun =
        fun(Port) ->
           gen_tcp:listen(Port,
                          [binary,
                           {ip, IP},
                           {packet, 0},
                           {active, false},
                           {reuseaddr, false},
                           {nodelay, true},
                           {keepalive, true},
                           {send_timeout, ?TCP_SEND_TIMEOUT},
                           {send_timeout_close, true}])
        end,
    OptsWithTLS =
        case Transport of
            tls ->
                [tls | Opts];
            tcp ->
                Opts
        end,
    PortInfo =
        if ClientPort == undefined ->
               {MinPort, MaxPort};
           true ->
               ClientPort
        end,
    case open_socket(PortInfo, OpenFun) of
        {ok, ListenSocket} ->
            {ok, PortNumber} = inet:port(ListenSocket),
            Owner ! {self(), {ok, PortNumber}},
            OptsWithTLS1 = stun:tcp_init(ListenSocket, OptsWithTLS),
            accept(ListenSocket, OptsWithTLS1);
        Err ->
            Owner ! {self(), Err}
    end;
start_listener(IP, ClientPort, {MinPort, MaxPort}, udp, Opts, Owner) ->
    OpenFun =
        fun(Port) -> gen_udp:open(Port, [binary, 
            {ip, IP}, 
            {active, false},
            {recbuf, ?UDP_RECBUF},
            {read_packets, ?UDP_READ_PACKETS}, 
            {reuseaddr, false}])
        end,
    PortInfo =
        if ClientPort == undefined ->
               {MinPort, MaxPort};
           true ->
               ClientPort
        end,
    case open_socket(PortInfo, OpenFun) of
        {ok, Socket} ->
            {ok, PortNumber} = inet:port(Socket),
            Owner ! {self(), {ok, PortNumber}},
            stun_logger:set_metadata(listener, udp),
            NewOpts = stun:udp_init(Socket, Opts),
            udp_recv(Socket, NewOpts);
        Err ->
            Owner ! {self(), Err}
    end.

accept(ListenSocket, Opts) ->
    Transport =
        case proplists:get_bool(tls, Opts) of
            true ->
                tls;
            false ->
                tcp
        end,
    ID = stun_logger:make_id(),
    stun_logger:set_metadata(listener, Transport, ID),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case {inet:peername(Socket), inet:sockname(Socket)} of
                {{ok, {PeerAddr, PeerPort}}, {ok, {Addr, Port}}} ->
                    ?LOG_INFO("Accepting connection: ~s -> ~s",
                              [stun_logger:encode_addr({PeerAddr, PeerPort}),
                               stun_logger:encode_addr({Addr, Port})]),
                    case stun:start({gen_tcp, Socket}, [{session_id, ID} | Opts]) of
                        {ok, Pid} ->
                            gen_tcp:controlling_process(Socket, Pid);
                        Err ->
                            Err
                    end;
                Err ->
                    ?LOG_ERROR("Cannot fetch peername: ~p", [Err]),
                    Err
            end,
            accept(ListenSocket, Opts);
        Err ->
            Err
    end.

udp_recv(Socket, Opts) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, Packet}} ->
            case catch stun:udp_recv(Socket, Addr, Port, Packet, Opts) of
                {'EXIT', Reason} ->
                    ?LOG_ERROR("Cannot process UDP packet:~n"
                               "** Source: ~s~n"
                               "** Reason: ~p~n** Packet: ~p",
                               [stun_logger:encode_addr({Addr, Port}), Reason, Packet]),
                    udp_recv(Socket, Opts);
                NewOpts ->
                    udp_recv(Socket, NewOpts)
            end;
        {error, Reason} ->
            ?LOG_ERROR("Unexpected UDP error: ~s", [inet:format_error(Reason)]),
            erlang:error(Reason)
    end.

format_listener_error(IP, MinPort, MaxPort, Transport, Opts, Err) ->
    ?LOG_ERROR("Cannot start listener:~n"
               "** IP: ~s~n"
               "** PortRage: ~B-~B~n"
               "** Transport: ~s~n"
               "** Options: ~p~n"
               "** Reason: ~p",
               [stun_logger:encode_addr(IP), MinPort, MaxPort, Transport, Opts, Err]).

open_socket({MinPort, MaxPort}, OpenSockFunc) ->
    Count = MaxPort - MinPort,
    Next = MinPort + stun:rand_uniform(Count + 1) - 1,
    open_socket(MinPort, MaxPort, OpenSockFunc, Next, Count);
open_socket(Port, OpenSockFunc) ->
    OpenSockFunc(Port).

open_socket(_MinPort, _MaxPort, _OpenSockFunc, _Next, -1) ->
    {error, eaddrinuse};
open_socket(MinPort, MaxPort, OpenSockFunc, Next, Count) ->
    case OpenSockFunc(Next) of
        {ok, Sock} ->
            {ok, Sock};
        {error, eaddrinuse} ->
            if Next == MaxPort ->
                   open_socket(MinPort, MaxPort, OpenSockFunc, MinPort, Count - 1);
               true ->
                   open_socket(MinPort, MaxPort, OpenSockFunc, Next + 1, Count - 1)
            end;
        Err ->
            Err
    end.
