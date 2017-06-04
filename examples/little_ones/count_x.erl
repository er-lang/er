-module(count_x).
-author('joe@cslab.ericssons.se').
%% http://www.erlang.org/article/10

%% count the number of x's in a file

-export([file/1]).

file(F) ->
    lists:foldl(fun($x,N) -> N + 1;
		   (_, N) -> N
	        end,
		0,
		binary_to_list(element(2, file:read_file(F)))).
