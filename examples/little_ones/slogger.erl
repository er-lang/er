%%% File    : slogger.erl
%%% Author  : Claes Wikstrom <klacke@jb.du.etx.ericsson.se>
%%% Purpose : a simple term logger
%%% Created : 24 Oct 1998 by Claes Wikstrom <klacke@jb.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(slogger).
-author('klacke@jb.du.etx.ericsson.se').

-export([start/0, stop/0, log/1, upread/1, truncate/0]).
-export([loop0/1]).

-import(klib, [i32/1, getint32/1]).

-define(LOGFILE, "slog.log").

start() ->
    start(?LOGFILE).

start(F) ->
    case whereis(?MODULE) of
	undefined ->
	    register(?MODULE, spawn(?MODULE, loop0, [F]));
	_Pid ->
	    true
    end.

stop()->
    req(stop).

req(R) ->
    ?MODULE ! {self(), R},
    receive
	{?MODULE, Reply} ->
	    Reply
    end.

loop0(FileName) ->
    case file:open(FileName, [read, write, raw, binary]) of
	{ok, Fd} ->
	    {ok, Eof} = file:position(Fd, eof),
	    file:position(Fd, bof),
	    FilePos = position_fd(Fd, 0),
	    maybe_warn(FilePos, Eof),
	    loop(Fd);
	{error, Reason} ->
	    warn("Can't open ~p~n", [FileName]),
	    exit(Reason)
    end.

maybe_warn(FilePos, Eof) ->
    if
	FilePos == Eof ->
	    ok;
	true ->
	    warn("~w bytes truncated \n",
		 [Eof - FilePos])
    end.


position_fd(Fd, LastPos) ->
    case catch getint32(Fd) of
	Int when is_integer(Int) ->
	    case file:read(Fd, Int) of
		{ok, B} when size(B) ==  Int ->
		    position_fd(Fd, LastPos + 4 + Int);
		_ ->
		    file:position(Fd, LastPos),
		    file:truncate(Fd)
	    end;
	_ ->
	    file:position(Fd, LastPos),
	    file:truncate(Fd),
	    LastPos
    end.

loop(Fd) ->
    receive
	{From, {log, Bin}} ->
	    From ! {?MODULE, log_binary(Fd, Bin)};
	{From, {upread, Fun}} ->
	    From ! {?MODULE, upread(Fd, Fun)};
	{From, truncate} ->
	    file:position(Fd, bof),
	    file:truncate(Fd),
	    From ! {?MODULE, ok};
	{From, stop} ->
	    file:close(Fd),
	    From ! {?MODULE, stopped},
	    exit(normal)
    end,
    loop(Fd).

log_binary(Fd, Bin) ->
    Sz = size(Bin),
    case file:write(Fd, [i32(Sz), Bin]) of
	ok ->
	    ok;
	{error, Reason} ->
	    warn("Cant't write logfile ~p ", [Reason]),
	    {error, Reason}
    end.


warn(Fmt, As) ->
    io:format(user, "slogger: " ++ Fmt, [As]).


upread(Fd, Fun) ->
    {ok, _Curr} = file:position(Fd, cur),
    file:position(Fd, bof),
    upread(Fd, get_term(Fd), Fun).

upread(_Fd, {'EXIT', _}, _Fun) ->
    ok;
upread(Fd, Term, Fun) ->
    Fun(Term),
    upread(Fd, catch get_term(Fd), Fun).


get_term(Fd) ->
    I = getint32(Fd),
    {ok, B} = file:read(Fd, I),
    binary_to_term(B).


%%% client functions
upread(Fun) ->
    req({upread, Fun}).

truncate() ->
    req(truncate).

log(Term) ->
    req({log, term_to_binary(Term)}).
