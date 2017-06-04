%%%----------------------------------------------------------------------
%%% File    : glogger.erl
%%% Author  : Claes Wikstrom <klacke@erix.ericsson.se>
%%% Purpose :
%%% Created :  3 Nov 1998 by Claes Wikstrom <klacke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(glogger).
-author('klacke@erix.ericsson.se').


-behaviour(gen_server).

%% External exports
-export([start_link/1, start_link/0, stop/0, log/1, upread/1, truncate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-import(klib, [i32/1, getint32/1]).

-define(LOGFILE, "slog.log").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    start_link(?LOGFILE).
start_link(Filename) ->
    gen_server:start_link({local, glogger}, glogger, Filename, []).

stop() ->
    gen_server:call(glogger, stop).

log(Term) ->
    gen_server:call(glogger, {log, term_to_binary(Term)}).

upread(Fun) ->
    gen_server:call(glogger, {upread, Fun}).

truncate() ->
    gen_server:call(glogger, truncate).



%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init(FileName) ->
     case file:open(FileName, [read, write, raw, binary]) of
	 {ok, Fd} ->
	     {ok, Eof} = file:position(Fd, eof),
	     file:position(Fd, bof),
	     FilePos = position_fd(Fd, 0),
	     maybe_warn(FilePos, Eof),
	     {ok, Fd};
	{error, Reason} ->
	    warn("Can't open ~p~n", [FileName]),
	     {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({log, Bin}, _From, Fd) ->
    {reply, log_binary(Fd, Bin) , Fd};

handle_call({upread, Fun}, _From, Fd) ->
    {reply, upread(Fd, Fun), fd};

handle_call(truncate, _From, Fd) ->
    file:position(Fd, bof),
    file:truncate(Fd),
    {reply, ok, Fd};

handle_call(stop, _, Fd) ->
    {stop, stopped, Fd}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, Fd) ->
    file:close(Fd).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

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
