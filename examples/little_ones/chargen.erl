%%%----------------------------------------------------------------------
%%% File    : chargen.erl
%%% Author  : Claes Wikstrom <klacke@erix.ericsson.se>
%%% Purpose :
%%% Created :  3 Nov 1998 by Claes Wikstrom <klacke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(chargen).
-author('klacke@erix.ericsson.se').

-export([start_link/0, loop0/1, worker/2]).

-define(PORTNO, 2019).

start_link() ->
    start_link(?PORTNO).
start_link(P) ->
    spawn_link(?MODULE, loop0, [P]).

loop0(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true},
			       {packet, 0}, {active, false}]) of
	{ok, LSock} ->
	    spawn(?MODULE, worker, [self(), LSock]),
	    loop(LSock);
	Other ->
	    io:format("Can't listen to socket ~p~n", [Other])
    end.

loop(S) ->
    receive
	next_worker ->
	    spawn_link(?MODULE, worker, [self(), S])
    end,
    loop(S).


worker(Server, LS) ->
    case gen_tcp:accept(LS) of
	{ok, Socket} ->
	    Server ! next_worker,
	    gen_chars(Socket, 32);
	{error, Reason} ->
	    Server ! next_worker,
	    io:format("Can't accept socket ~p~n", [Reason])
    end.


gen_chars(Socket, Char) ->
    Line = make_line(Char, 0),
    case gen_tcp:send(Socket, Line) of
	{error, _Reason} -> exit(normal);
	ok -> gen_chars(Socket, upchar(Char))
    end.


make_line(_Char, 70) ->
    [10];
make_line(127, Num) ->
    make_line(32, Num);
make_line(Char, Num) ->
    [Char | make_line(Char+1, Num+1)].

upchar(Char) ->
    if
	Char + 70 > 127 ->
	    32 + (127 - Char);
	true ->
	    Char + 70
    end.
