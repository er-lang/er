%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ift).

%% ift: 

-export([ update_erlcinfo1/3
        , update_erlcinfo2/3
        ]).


%% API

update_erlcinfo1(G, Source, Dirs) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            LastModified = filelib:last_modified(Source),
            if LastModified == 0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    modified;
               LastUpdated < LastModified ->
                    m:modify_erlcinfo(G, Source, Dirs),
                    modified;
               true ->
                    unmodified
            end;
        false ->
            m:modify_erlcinfo(G, Source, Dirs),
            modified
    end.

update_erlcinfo2(G, Source, Dirs) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            LastModified = filelib:last_modified(Source),
            if LastModified == 0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    modified;
               LastUpdated < LastModified ->
                    m:modify_erlcinfo(G, Source, Dirs);
               modified;
               true ->
                    unmodified
            end;
        false ->
            m:modify_erlcinfo(G, Source, Dirs),
            modified
    end.

%% Internals

%% End of Module.
