%%%----------------------------------------------------------------------
%%% File    : time_server.erl
%%% Author  : Claes Wikstrom <klacke@erix.ericsson.se>
%%% Purpose :
%%% Created :  3 Nov 1998 by Claes Wikstrom <klacke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(time_server).
-author('klacke@erix.ericsson.se').

-export([start/0, loop0/1]).

-define(PORTNO, 2345).

start() ->
    start(?PORTNO).
start(Pno) ->
    spawn(?MODULE, loop0, [Pno]).

loop0(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
	{ok, LSock} ->
	    loop(LSock);
	_ ->
	    stop
    end.

loop(Listen) ->
    case gen_tcp:accept(Listen) of
	{ok, S} ->
	    gen_tcp:send(S, io_lib:format("~p~n", [{date(), time()}])),
	    gen_tcp:close(S),
	    loop(Listen);
	_ ->
	    loop(Listen)
    end.
