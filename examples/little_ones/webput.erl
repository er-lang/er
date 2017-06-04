-module(webput).
%% http://www.erlang.org/article/9

-doc([{author, 'Joe Armstrong'},
      {title, "Publish data on a web site"},
      {keywords,[web,www,home,page,publish]},
      {date, 981112}]).

-export([publish/5]).

publish(Host, User, Password, LocalDir, RemoteDir) ->
    case ftp:open(Host) of
	{ok, Pid} ->
	    case ftp:user(Pid, User, Password) of
		ok ->
		    case ftp:cd(Pid, RemoteDir) of
			ok ->
			    case file:list_dir(LocalDir) of
				{ok, Files} ->
				    lists:foreach(fun(I) ->
						   publish(I, LocalDir, Pid)
					   end, Files);
				{error, _} ->
				    exit({bad,local,directory, LocalDir})
			    end;
			{error, Reason} ->
			    exit({cannot,cd,to,RemoteDir,reason,Reason})
		    end;
		{error, Reason} ->
		    exit({cannot, login, as, User, reason, Reason})
	    end;
	{error, Reason} ->
	    exit({cannot,connect,to,Host, reason, Reason})
    end.

publish(File, Dir, Pid) ->
    LocalFile = Dir ++ "/" ++ File,
    case ftp:send(Pid, LocalFile, File) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit({cannot,send,file,File,reason,Reason})
    end.
