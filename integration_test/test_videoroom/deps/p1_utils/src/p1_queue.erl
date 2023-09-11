%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017-2021 Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(p1_queue).

%% API
-export([new/0, new/1, new/2, is_queue/1, len/1, is_empty/1, in/2, out/1,
	 peek/1, drop/1, from_list/1, from_list/2, from_list/3,
	 to_list/1, clear/1, foreach/2, foldl/3, dropwhile/2, type/1,
	 format_error/1, ram_to_file/1, file_to_ram/1, get_limit/1,
	 set_limit/2]).
-export([start/1, stop/0]).

-type limit() :: non_neg_integer() | unlimited.
-type rqueue() :: rqueue(any()).
-type rqueue(T) :: {queue:queue(T), non_neg_integer(), limit()}.
-type fqueue() :: p1_file_queue:queue().
-type queue() :: rqueue(any()) | fqueue().
-type queue(T) :: rqueue(T) | fqueue().
-type queue_type() :: ram | file.
-type error_reason() :: p1_file_queue:error_reason().
-export_type([queue/0, queue/1, queue_type/0, error_reason/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(file:filename()) -> ok | {error, any()}.
start(Dir) ->
    application:ensure_all_started(p1_utils),
    case p1_file_queue:start(Dir) of
	{ok, _} -> ok;
	{error, {already_started, _}} -> ok;
	Err -> Err
    end.

-spec stop() -> ok | {error, any()}.
stop() ->
    p1_file_queue:stop().

-spec new() -> rqueue().
new() ->
    new(ram).

-spec new(ram) -> rqueue();
	 (file) -> fqueue().
new(Type) ->
    new(Type, unlimited).

-spec new(ram, limit()) -> rqueue();
	 (file, limit()) -> fqueue().
new(ram, Limit) ->
    {queue:new(), 0, Limit};
new(file, Limit) ->
    p1_file_queue:new(Limit).

-spec type(queue()) -> ram | {file, file:filename()}.
type({_, _, _}) ->
    ram;
type(Q) ->
    {file, p1_file_queue:path(Q)}.

-spec is_queue(any()) -> boolean().
is_queue({Q, Len, _}) when is_integer(Len), Len >= 0 ->
    queue:is_queue(Q);
is_queue(Q) ->
    p1_file_queue:is_queue(Q).

-spec len(queue()) -> non_neg_integer().
len({_, Len, _}) ->
    Len;
len(Q) ->
    p1_file_queue:len(Q).

-spec is_empty(queue()) -> boolean().
is_empty({_, Len, _}) ->
    Len == 0;
is_empty(Q) ->
    p1_file_queue:is_empty(Q).

-spec get_limit(queue()) -> limit().
get_limit({_, _, Limit}) ->
    Limit;
get_limit(Q) ->
    p1_file_queue:get_limit(Q).

-spec set_limit(rqueue(T), limit()) -> rqueue(T);
	       (fqueue(), limit()) -> fqueue().
set_limit({Q, Len, _}, Limit) ->
    {Q, Len, Limit};
set_limit(Q, Limit) ->
    p1_file_queue:set_limit(Q, Limit).

-spec in(term(), rqueue(T)) -> rqueue(T);
	(term(), fqueue()) -> fqueue().
in(Item, {Q, Len, Limit}) ->
    if Len < Limit ->
	    {queue:in(Item, Q), Len+1, Limit};
       true ->
	    erlang:error(full)
    end;
in(Item, Q) ->
    p1_file_queue:in(Item, Q).

-spec out(rqueue(T)) -> {{value, term()}, rqueue(T)} | {empty, rqueue(T)};
	 (fqueue()) -> {{value, term()}, fqueue()} | {empty, fqueue()}.
out({Q, 0, Limit}) ->
    {empty, {Q, 0, Limit}};
out({Q, Len, Limit}) ->
    {{value, Item}, Q1} = queue:out(Q),
    {{value, Item}, {Q1, Len-1, Limit}};
out(Q) ->
    p1_file_queue:out(Q).

-spec peek(queue(T)) -> empty | {value, T}.
peek({Q, _, _}) ->
    queue:peek(Q);
peek(Q) ->
    p1_file_queue:peek(Q).

-spec drop(rqueue(T)) -> rqueue(T);
	  (fqueue()) -> fqueue().
drop({Q, Len, Limit}) ->
    {queue:drop(Q), Len-1, Limit};
drop(Q) ->
    p1_file_queue:drop(Q).

-spec from_list([T]) -> rqueue(T).
from_list(L) ->
    from_list(L, ram, unlimited).

-spec from_list([T], ram) -> rqueue(T);
	       (list(), file) -> fqueue().
from_list(L, Type) ->
    from_list(L, Type, unlimited).

-spec from_list([T], ram, limit()) -> rqueue(T);
	       (list(), file, limit()) -> fqueue().
from_list(L, ram, Limit) ->
    Len = length(L),
    if Len =< Limit ->
	    {queue:from_list(L), Len, Limit};
       true ->
	    erlang:error(full)
    end;
from_list(L, file, Limit) ->
    p1_file_queue:from_list(L, Limit).

-spec to_list(queue(T)) -> [T].
to_list({Q, _, _}) ->
    queue:to_list(Q);
to_list(Q) ->
    p1_file_queue:to_list(Q).

-spec foreach(fun((T) -> term()), queue(T)) -> ok.
foreach(F, {Q, Len, Limit}) ->
    case queue:out(Q) of
	{{value, Item}, Q1} ->
	    F(Item),
	    foreach(F, {Q1, Len-1, Limit});
	{empty, _} ->
	    ok
    end;
foreach(F, Q) ->
    p1_file_queue:foreach(F, Q).

-spec foldl(fun((T1, T2) -> T2), T2, queue(T1)) -> T2.
foldl(F, Acc, {Q, Len, Limit}) ->
    case queue:out(Q) of
	{{value, Item}, Q1} ->
	    Acc1 = F(Item, Acc),
	    foldl(F, Acc1, {Q1, Len-1, Limit});
	{empty, _} ->
	    Acc
    end;
foldl(F, Acc, Q) ->
    p1_file_queue:foldl(F, Acc, Q).

-spec dropwhile(fun((T) -> boolean()), rqueue(T)) -> rqueue(T);
		(fun((term()) -> boolean()), fqueue()) -> fqueue().
dropwhile(_, {_, 0, _} = Q) ->
    Q;
dropwhile(F, {Q, Len, Limit}) ->
    {value, Item} = queue:peek(Q),
    case F(Item) of
	true ->
	    dropwhile(F, {queue:drop(Q), Len-1, Limit});
	_ ->
	    {Q, Len, Limit}
    end;
dropwhile(F, Q) ->
    p1_file_queue:dropwhile(F, Q).

-spec clear(rqueue(T)) -> rqueue(T);
	   (fqueue()) -> fqueue().
clear({_, _, Limit}) ->
    {queue:new(), 0, Limit};
clear(Q) ->
    p1_file_queue:clear(Q).

-spec ram_to_file(queue()) -> fqueue().
ram_to_file({_, _, Limit} = Q) ->
    foldl(fun p1_file_queue:in/2, new(file, Limit), Q);
ram_to_file(Q) ->
    Q.

-spec file_to_ram(queue()) -> rqueue().
file_to_ram({_, _, _} = Q) ->
    Q;
file_to_ram(Q) ->
    Limit = p1_file_queue:get_limit(Q),
    p1_file_queue:foldl(fun in/2, new(ram, Limit), Q).

-spec format_error(error_reason()) -> string().
format_error(Reason) ->
    p1_file_queue:format_error(Reason).

%%%===================================================================
%%% Internal functions
%%%===================================================================
