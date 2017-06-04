%%%----------------------------------------------------------------------
%%% File    : count_chars.erl
%%% Author  : Claes Wikstrom <klacke@erix.ericsson.se>
%%% Purpose : Count the x chars in a file
%%% Created : 20 Oct 1998 by Claes Wikstrom <klacke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(count_chars).
-author('klacke@erix.ericsson.se').

-export([file/1, file1/1, file2/1]).


file(Fname) ->
    case file:open(Fname, [read, raw, binary]) of
	{ok, Fd} ->
	    scan_file(Fd, 0, file:read(Fd, 1024));
	{error, Reason} ->
	    {error, Reason}
    end.

scan_file(Fd, Occurs, {ok, Binary}) ->
    scan_file(Fd, Occurs + count_x(Binary), file:read(Fd, 1024));
scan_file(Fd, Occurs, eof) ->
    file:close(Fd),
    Occurs;
scan_file(Fd, _Occurs, {error, Reason}) ->
    file:close(Fd),
    {error, Reason}.


%% Count the number of 'x' chars in a binary

count_x(Bin) ->
    count_x(binary_to_list(Bin), 0).
count_x([$x|Tail], Ack) ->
    count_x(Tail, Ack+1);
count_x([_|Tail], Ack) ->
    count_x(Tail, Ack);
count_x([], Ack) ->
    Ack.



file1(File) ->
    F = fun(Bin, Int) ->
		{more, count_x(Bin) + Int}
	end,
    klib:with_file(File, F, 0).


file2(File) ->
    {ok,B} = file:read_file(File),
    lists:foldl(fun($x, Ack) ->
			1 + Ack;
		   (_, Ack) ->
			Ack
		end, 0, binary_to_list(B)).
