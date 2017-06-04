-module(wc).
-author('klacke@erix.ericsson.se').

-import(count_chars, [with_file/3]).
-import(lists, [map/2, foreach/2]).

-export([file/1, files/1]).

file(File) ->
    output([gfile(File)]).

gfile(File) ->
    Fun = fun(Bin, Count) ->
		  count_bin(binary_to_list(Bin), inspace, Count)
	  end,
    {File, with_file(File, Fun, {0,0,0})}.


count_bin([H|T], Where, {C,W,L}) ->
    case classify_char(H) of
	newline  when Where == inspace ->
	    count_bin(T, inspace, {C+1, W, L+1});
	newline when Where == inword ->
	    count_bin(T, inspace, {C+1, W+1, L+1});
	space  when Where == inspace ->
	    count_bin(T, inspace, {C+1, W, L});
	space  when Where == inword ->
	    count_bin(T, inspace, {C+1, W+1, L});
	char ->
	    count_bin(T, inword, {C+1, W, L})
    end;
count_bin([], inword, {C, W, L}) ->
    {more, {C, W+1, L}};
count_bin([], inspace, {C, W, L}) ->
    {more, {C, W, L}}.


classify_char($ ) ->
    space;
classify_char($\t) ->
    space;
classify_char($\n) ->
    newline;
classify_char(_) ->
    char.

files(Files) ->
    output(map(fun(F) -> gfile(F) end, Files)).

output(Counts) ->
    io:format("~-25s ~-10s ~-10s ~-10s~n",
	      ["file", "chars", "words", "lines"]),
    foreach(fun({File, {C,W,L}}) ->
		    ok = io:format("~-25s ~-10w ~-10w ~-10w~n",
			      [File, C, W, L])
	    end, Counts).
