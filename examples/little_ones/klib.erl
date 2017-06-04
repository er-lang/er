-module(klib).
-author('klacke@jb.du.etx.ericsson.se').
%% My very own lib of nice to have functions.


-export([with_file/3]).
-export([i32/1, getint32/1]).
-export([bits/1, bits/2, hex/1, hex/2]).
-export([with_socket/2,  sock_status/1]).



%% internal exports
-export([with_socket0/2, sock_handler/3]).


%% A general purpose with_file function

with_file(File, Fun, Initial) ->
    case file:open(File, [read, raw, binary]) of
	{ok, Fd} ->
	    Res = feed(Fd, file:read(Fd, 1024), Fun, Initial),
	    file:close(Fd),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

feed(Fd, {ok, Bin}, Fun, Farg) ->
    case Fun(Bin, Farg) of
	{done, Res} ->
	    Res;
	{more, Ack} ->
	    feed(Fd, file:read(Fd, 1024), Fun, Ack)
    end;
feed(_Fd, eof, _Fun, Ack) ->
    Ack;
feed(_Fd, {error, Reason}, _Fun, _Ack) ->
    {error, Reason}.




%% make/unmake ints
i32(B) when is_binary(B) ->
    i32(binary_to_list(B, 1, 4));
i32([X1, X2, X3, X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4;
i32(Int) when is_integer(Int) ->
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255].



%% Read an int from a file
getint32(F) ->
    {ok, B} = file:read(F, 4),
    i32(B).



%%% Purpose : return Integers niceley formatted as bits or hex
%%%           Handy debug functions when dealing with BIT
%%            formated ints, such as PDU's or tagged pointers


%% Bits display code

%% one byte nicely as a list of one string
bits(I) when is_integer(I), I < 256 ->
    [byte_to_bits(I)];

%% several bytes nicely as a list of strings
bits(I) when is_integer(I) ->
    byte_bits(I, get_lo_byte(I), []);

bits(I) when is_atom(I) ->
    bits(hex_to_int(I));
bits(I) when is_list(I) ->
    bits(hex_to_int(I)).

%% preset field lengths
%% typically, tagged 32 bit pointer is bits(PTR, [4, 28])
%% returns a list of strings
bits(I, Fields) when is_integer(I) ->
    [H|T] = lists:reverse(Fields),
    {Hi, Lo} = get_lo_bits(I, H),
    bit_fields(I, {Hi, Lo}, H, T, []);


bits(I, Fields) when is_atom(I) ->
    bits(hex_to_int(I), Fields);
bits(I, Fields) when is_list(I) ->
    bits(hex_to_int(I), Fields).


hex_to_int(H) when is_atom(H) ->
    hex_to_int(atom_to_list(H));
hex_to_int([$0, $x | Chars]) ->
    hex_to_int2(lists:reverse(Chars), 0, 0).

hex_to_int2([], _, Ack) ->
    Ack;
hex_to_int2([Char|Tail], Shift, Ack) ->
    Int = hchar_to_int(Char),
    hex_to_int2(Tail, Shift+4, Ack bor (Int bsl Shift)).

hchar_to_int(Char) when $0 =< Char, Char =< $9 ->
    Char - $0;
hchar_to_int(Char) when $A =< Char, Char =< $F ->
    Char - $A;
hchar_to_int(Char) when $a =< Char, Char =< $f ->
    Char - $a.


%% Last field
bit_fields(_Int, {0, Lo}, Field, [], Ack) ->
    Str = byte_to_bits(Lo, Field),
    [Str | Ack];

%% Int too large
bit_fields(_Int, {Hi, Lo}, Field, [], Ack) ->
    Str = byte_to_bits(Lo, Field),
    [{too_large_int, remain, Hi}, Str | Ack];

%% Lo is the Field lsb of Int
bit_fields(_Int, {Hi, Lo}, Field, [Next|Tail], Ack) ->
    Str = byte_to_bits(Lo, Field),
    bit_fields(Hi, get_lo_bits(Hi, Next), Next, Tail, [Str | Ack]).

byte_bits(_I, {0, 0}, LowAck) ->
    LowAck;
byte_bits(_I, {0, Lo}, LowAck) ->
    Bits = byte_to_bits(Lo),
    [Bits | LowAck];
byte_bits(_I, {Hi, Lo}, LowAck) ->
    Bits = byte_to_bits(Lo),
    byte_bits(Hi, get_lo_byte(Hi), [Bits | LowAck]).

get_lo_byte(Int) ->
    get_lo_bits(Int, 8).

get_lo_bits(Int, Size) ->
    Lo = Int band ones_mask(Size),
    Hi = Int bsr Size,
    {Hi, Lo}.

ones_mask(Ones) ->
    (1 bsl Ones) - 1.

byte_to_bits(Byte) ->
    byte_to_bits(Byte, 8).

byte_to_bits(Byte, Fill) ->
    byte_to_bits(Byte, Fill, []).
byte_to_bits(Byte, Fill, Ack) when Byte > 0 ->
    if
	(Byte band 1) == 1  ->
	    byte_to_bits(Byte bsr 1 , Fill, [1 | Ack] );
	true ->
	    byte_to_bits(Byte bsr 1 , Fill, [0 | Ack] )
    end;
byte_to_bits(0, Fill, Ack) ->
    eight_pad(Fill, Ack).

eight_pad(Fill, L) when is_integer(Fill) ->
    Len = length(L),
    lists:map(fun(Char) -> Char + $0 end,
	      lists:duplicate(Fill - Len, 0) ++ L);
eight_pad(nofill, L) ->
    lists:map(fun(Char) -> Char + $0 end, L).


%% Hex display code
hex(Int) ->
    hexi(get_lo_bits(Int, 4), []).

hexi({0, Lo}, Ack) ->
    [hex4(Lo) | Ack];
hexi({Hi, Lo} , Ack) ->
    hexi(get_lo_bits(Hi, 4), [hex4(Lo) | Ack]).

hex4(Int) when Int < 10 -> Int + $0;
hex4(Int) -> ($A - 10) + Int.


hex(Int, Fields) ->
    [H|T] = lists:reverse(Fields),
    hexf(get_lo_bits(Int, H), H, T, []).

hexf(_, Flen, _,_) when Flen rem 4 /= 0 ->
    exit(bad_field_len);
hexf({0, Lo}, Flen, [], Ack) ->
    Str0 = hex(Lo),
    Str = pad4(Str0, Flen, length(Str0)),
    [Str | Ack];

hexf({Hi, Lo}, Flen, [], Ack) ->
    Str0 = hex(Lo),
    Str = pad4(Str0, Flen, length(Str0)),
    [{too_large_int, remain, Hi}, Str | Ack];

hexf({Hi, Lo}, Flen, Tail, Ack) ->
    Str0 = hex(Lo),
    Str = pad4(Str0, Flen, length(Str0)),
    [H|T] = Tail,
    hexf(get_lo_bits(Hi, H), H, T, [Str |Ack]).

pad4(Str, Flen, Strlen) ->
    Chars = Flen div 4,
    lists:duplicate(noneg(Chars - Strlen), $0) ++ Str.

noneg(X) when X > 0 ->
     X;
noneg(_) ->
     0.



%% This function doesn't return, it listens to port
%% and process tcp requests on the port number.
%% Fun may return either of the following

%% done: socket is closed
%% {more, Ack} : Fun needs more data to process.
%% {done, Data} : Data is replied on the socket which is then closed:
%% continue: contine to process data:
%% {continue, Data}, Data is replied on the socket, and the server continues
%% {error, Reason}: socket is close and Reason logged

%% For en example a server that needs two lines of input
%% and then echoes a "hello" need to provide the following Fun
%% F = fun(Data) ->
%%       case Data of
%%         <X/binary, $\n/char, Y/binary, $\n/char | _>) ->
%%           {done, <"hello">};
%%         Other ->
%%          {more, Ack}
%%      end
%%   end.
%%
%% so to create a socket server which performs the above service
%% we need to call klib:with_socket(Port, F).
%% The server is a concurrent server so each
%% request runs in its own process.

with_socket(Port, Fun) ->
    spawn(?MODULE, with_socket0, [Port, Fun]).

with_socket0(Port, Fun) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {packet, 0},
			       {reuseaddr, true},
			       {active, false}]) of
	{ok, LSock} ->
	    P = spawn_link(?MODULE, sock_handler, [self(), Fun, LSock]),
	    sock_loop(P, Fun, 1, nil, LSock);
	Other ->
	    Other
    end.

sock_status(Pid) ->
    Pid ! {self(), status},
    receive
	{Pid, Status} ->
	    Status
    after 1000 ->
	    timeout
    end.



sock_loop(P, Fun, Procs, LastError, LSock) ->
    receive
	{P, connected} ->
	    P2 = spawn_link(?MODULE, sock_handler, [self(), Fun, LSock]),
	    sock_loop(P2, Fun, Procs+1, LastError, LSock);
	{'EXIT', P, {lasterror, Reason}} ->
	    P2 = spawn_link(?MODULE, sock_handler, [self(), Fun, LSock]),
	    sock_loop(P2, Fun, Procs, Reason, LSock);
	{'EXIT', _, {lasterror, Reason}} ->
	    sock_loop(P, Fun, Procs-1, Reason, LSock);
	{'EXIT', _, _} ->
	    sock_loop(P, Fun, Procs-1, LastError, LSock);
	{From, status} ->
	    From ! {self(), {ok, [{procs, Procs}, {lasterr, LastError}]}},
	    sock_loop(P, Fun, Procs, nil, LSock);
	{_From, stop} ->
	    exit(stop) %% propagate EXIT to all Pids
    end.


sock_handler(Top, Fun, S) ->
    case gen_tcp:accept(S) of
	{ok, Socket} ->
	    Top ! {self(), connected},
	    hloop(Socket, Fun, list_to_binary([])) ;
	{error, Reason} ->
	    lasterror({noaccept, S, Reason})
    end.

hloop(S, Fun, Ack) ->
    case gen_tcp:recv(S, 0) of
	{ok, B} when size(Ack) == 0 ->
	    process_data(S, Fun, B);
	{ok, B}  ->
	    process_data(S, Fun, erlang:concat_binary([B, Ack]));
	{error, Reason} ->
	    lasterror({badread, Reason})
    end.

lasterror(R) ->
    exit({lasterror, R}).

process_data(S, Fun, B) ->
    case Fun(B) of
	done ->
	    gen_tcp:close(S),
	    exit(normal);
	{more, Ack} ->
	    hloop(S, Fun, Ack);
	{done, Data} ->
	    case gen_tcp:send(S, Data) of
		ok ->
		    gen_tcp:close(S),
		    exit(normal);
		{error, Reason} ->
		    lasterror({badwrite, Reason})
	    end;
	continue ->
	    hloop(S, Fun, list_to_binary([]));
	{continue, Data} ->
	    case gen_tcp:send(S, Data) of
		ok ->
		    hloop(S, Fun, list_to_binary([]));
		{error, Reason} ->
		    lasterror({badwrite2, Reason})
	    end;
	{again, Data} ->
	     case gen_tcp:send(S, Data) of
		ok ->
		     ignor
	     end;
	{error, Reason} ->
	    lasterror({user_error, Reason});
	Other ->
	    lasterror({bad_ret_from_fun, Other})
    end.
