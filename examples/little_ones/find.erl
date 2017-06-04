-module(find).
-author('klacke@erix.ericsson.se').
-include_lib("kernel/include/file.hrl").

-export([files/3]).

%% Top is the Top directory where everything starts
%% Re is a regular expression to match for (see module regexp)
%% Actions is a Fun to apply to each found file
%% Return value is a lists of the return values from the
%% Action function

%% Example: find:files("/home/klacke",
%%                     ".*\.erl", fun(F) -> {File, c:c(File)} end)
%% Will find all erlang files in my top dir, compile them and
%% return a long list of {File, CompilationResult} tuples
%% If an error occurs, {error, {File, Reason}} is returned
%% The Action fun is passed the full long file name as parameter


files(Top, Re, Action) ->
    case file:list_dir(Top) of
	{ok, Files} ->
	    files(Top, Files, Re, Action, []);
	{error, Reason}  ->
	    {error, {Top, Reason}}
    end.

files(Top, [F|Tail], Re, Action, Ack) ->
    F2 = Top ++ "/" ++ F,
    case file:read_file_info(F2) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    case files(F2, Re, Action) of
		{error, Reason} ->
		    {error, Reason};
		List ->
		    files(Top, Tail, Re, Action, List ++ Ack)
	    end;
	{error, Reason} ->
	    {error, {F2, Reason}};
	{ok, FileInfo} when FileInfo#file_info.type == regular ->
	    case catch regexp:match(F, Re) of
		{match, _,_} ->
		    files(Top, Tail, Re, Action, [Action(F2) | Ack]);
		nomatch ->
		    files(Top, Tail, Re, Action, Ack);
		{error, Reason} ->
		    {error, {F2, {regexp, Reason}}}
	    end;
	_Other ->
	    files(Top, Tail, Re, Action, Ack)
    end;

files(_Top, [], _Re, _Action, Ack) ->
    Ack.
