-module(urlget).

%% http://www.erlang.org/article/8
%% Hacked by Roland and Erik Aug 1997

%% Joe Armstrong

%% get_http(Fun, URL, OPts, Proxy, Timeout) ->
%%      ok{URL', Header, Body} | error{What}
%% URL' is the actual URL that was gotten

-export([test/1, direct/1, proxy/1, get_http/5]).

-import(lists, [reverse/1]).

test(1) -> direct("http://www.ericsson.se/cslab/~joe");
test(2) -> direct("http://www.viasat.se/index9.html");
test(3) -> proxy("http://www.ericsson.se/cslab/~joe");
test(4) -> proxy("http://www.viasat.se/index9.html").


direct(URL) ->
    Proxy   =  noproxy,
    Timeout = 60000,
    case get_http(fun progress/2, URL, [], Proxy, Timeout) of
        {ok,{Url,Reply,Head,Body}} ->
            {ok,{Url,Reply,Head,binary_to_list(Body)}};
        Other ->
            Other
    end.

proxy(URL) ->
    Proxy   =  {"proxy", 82},
    Timeout = 60000,
    case get_http(fun progress/2, URL, [], Proxy, Timeout) of
        {ok,{Url,Reply,Head,Body}} ->
            {ok,{Url,Reply,Head,binary_to_list(Body)}};
        Other ->
            Other
    end.

progress(not_known, Y) ->
    io:format("progress:#bytes = ~p~n", [Y]);
progress({length,N}, Y) ->
    K = trunc(Y*100/N),
    io:format("progress:#bytes (~w\%) = ~p~n", [K, Y]).

get_http(Fun, URL, Opts, Proxy, Timeout) ->
    %% io:format("url_server: URL ~p~n", [URL]),
    case get_ip_port(URL,Proxy) of
	{ok, {IP, Port, Url0}} ->
	    Cmd = ["GET ", Url0, " HTTP/1.0\r\n", Opts, "\r\n"],
	    get_http2(Fun, URL, IP, Port, Cmd, Opts, Proxy, Timeout);
	Other ->
	    Other
    end.

get_http2(Fun, URL, IP, Port, Cmd, Opts, Proxy, Timeout) ->
    io:format("Here connect:~p ~p~n",[IP, Port]),
    case gen_tcp:connect(IP, Port, [binary, {packet,0}]) of
	{error, Why} ->
	    {error, {socket_error, Why}};
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, Cmd),
	    Return = case receive_header(Fun,URL,list_to_binary([]),
					 Socket,Timeout) of
			 {redo, URL1} ->
			     get_http(Fun, URL1, Opts, Proxy, Timeout);
			 Other ->
			     Other
		     end,
	    ok = gen_tcp:close(Socket),
	    Return
    end.

get_ip_port(URL, Proxy) ->
    case Proxy of
	noproxy ->
	    case parse(URL) of
		{error, Why} ->
		    {error, {badURL,{Why,URL}}};
		{http, IP, Port, Url0} ->
		    {ok, {IP, Port,Url0}};
		Other ->
		    {error,{unknown,Other}}
	    end;
	{IP,Port} ->
	    {ok,{IP,Port,URL}}
    end.

receive_header(Fun, URL, Bin, Socket, Timeout) ->
    receive
	{tcp, Socket, B} ->
	    B1 =  concat_binary([Bin,B]),
	    case get_header(B1) of
		{ok, Reply, Header, BT} ->
		    Size = content_length(Header),
		    case get_field(Header,"Location") of
			{true,URL1} ->
			    %% If it's redo we still have to get the body
			    %% to flush the socket
			    case receive_body(Fun,Size,BT,Socket,Timeout) of
				{ok, Body} ->
				    {redo, URL1};
				Error ->
				    Error
			    end;
			_ ->
			    case receive_body(Fun,Size,BT,Socket,Timeout) of
				{ok, Body} ->
				    {ok, {URL, Reply, Header, Body}};
				Error ->
				    Error
			    end
		    end;
		more ->
		    receive_header(Fun, URL, B1, Socket, Timeout)
	    end;
	{tcp_closed, Socket} ->
	    {error, socket_closed_in_header};
	{tcp_error, Socket, Reason} ->
	    {error, Reason};
	Other ->
	    {error, {socket, Other}}
	after
	    Timeout ->
		{error, timeout}
    end.

receive_body(Fun, Size, Bin, Socket, Timeout) ->
    receive
	{tcp, Socket, B} ->
	    B1 = concat_binary([Bin,B]),
	    Fun(Size, size(B1)),
	    receive_body(Fun, Size, concat_binary([Bin,B]), Socket, Timeout);
	{tcp_closed, Socket} ->
	    {ok, Bin};
	{tcp_error, Socket, What} ->
	    {error, {socket, What}};
	Other ->
	    {error, {socket, Other}}
	after
	    Timeout ->
		{error, timeout}
    end.

get_header(B) ->
    L = binary_to_list(B),
    case split_header(L, []) of
	{ReplyHeader, Rest} ->
	    {Reply,Header} = parse_reply(ReplyHeader),
	    {ok, Reply, parse_header(Header), list_to_binary(Rest)};
	fail ->
	    more
    end.

split_header([$\r,$\n,$\r,$\n|T], L) -> {reverse(L), T};
split_header([$\n,$\n|T], L)         -> {reverse(L), T};
split_header([H|T], L)               -> split_header(T, [H|L]);
split_header([], L)                  -> fail.

get_field([{K,V}|T],K) -> {true,V};
get_field([_|T],K)     -> get_field(T,K);
get_field([],_)        -> {false,false}.

parse_reply(R0) ->
    {HTTP,R1} = get_until(R0,$ ,[]),
    {CODE,R2} = get_until(R1,$ ,[]),
    {COMM,R3} = get_until(R2,$\n,[]),
    {{trim(HTTP),list_to_integer(trim(CODE)),trim(COMM)},R3}.

get_until([R|Rs],R,L) ->
    {reverse(L),Rs};
get_until([R|Rs],P,L) ->
    get_until(Rs,P,[R|L]).

parse_header(T) ->
    {_, P} = parse_header(T, []),
    P.

% It looks like parse_header/2 includes split header ???? - roland
parse_header([$\r,$\n | T], Info) -> header_end(T, Info);
parse_header([$\n | T], Info)     -> header_end(T, Info);
parse_header(Cs, Info)            -> header_line(Cs, [], Info).

header_line([$\r,$\n | T], Acc, Info) ->
    parse_header(T, [split_info(reverse(Acc)) | Info]);
header_line([$\n | T], Acc, Info) ->
    parse_header(T, [split_info(reverse(Acc)) | Info]);
header_line([C | Cs], Acc, Info) ->
    header_line(Cs, [C | Acc], Info);
header_line([], Acc, Info) ->
    header_end([], [split_info(reverse(Acc)) | Info]).

header_end([$\r,$\n | T], Info) -> header_end(T, Info);
header_end([$\n | T], Info)     -> header_end(T, Info);
header_end(T, Info)             -> {T, Info}.

split_info(String) ->
    case string:chr(String, $:) of
	0 -> {"Parse-Error",trim(String)};
	Ix ->
	    {trim(string:substr(String, 1, Ix-1)),
	     trim(string:substr(String, Ix+1, length(String)))}
    end.

trim(String) ->
    reverse(strip(reverse(strip(String)))).

strip([$   | Cs]) -> strip(Cs);
strip([$\t | Cs]) -> strip(Cs);
strip([$\r | Cs]) -> strip(Cs);
strip([$\n | Cs]) -> strip(Cs);
strip(Cs) -> Cs.

content_length(Header) ->
    case get_field(Header, "Content-Length") of
	{true, Str} ->
	    {length, list_to_integer(Str)};
	{false, _} ->
	    not_known
    end.

%%----------------------------------------------------------------------
%% parse(URL) -> {http, Site, Port, File} |
%%               {file, File}             | {error,Why}
%% (primitive)

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse_http(T);
parse([$f,$t,$p,$:,$/,$/|T])    ->  {error, no_ftp};
parse([$f,$i,$l,$e,$:,$/,$/|F]) ->  {file, F};
parse(X)                        ->  {error, unknown_url_type}.

parse_http(X) ->
    case string:chr(X, $/) of
	0 ->
	    %% not terminated by "/" (sigh)
	    %% try again
	    parse_http(X ++ "/");
	N ->
	    %% The Host is up to the first "/"
	    %% The file is everything else
	    Host = string:substr(X, 1, N-1),
	    File = string:substr(X, N, length(X)),
	    %% Now check to see if the host name contains a colon
	    %% i.e. there is an explicit port address in the hostname
	    case string:chr(Host, $:) of
		0 ->
		    %% no colon
		    Port = 80,
		    {http, Host, 80, File};
		M ->
		    Site = string:substr(Host,1,M-1),
		    case (catch list_to_integer(
				  string:substr(Host, M+1, length(Host)))) of
			{'EXIT', _} ->
			    {http, Site, 80, File};
			Port ->
			    {http, Site, Port, File}
		    end
	    end
    end.
