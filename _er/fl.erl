%% Copyright © 2013 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(fl).

%% fl: flatlength

-export([flatlength/1]).

%% API

%% flatlength(List)
%%  Calculate the length of a list of lists.

-spec flatlength(DeepList) -> non_neg_integer() when
      DeepList :: [term() | DeepList].

flatlength(List) ->
    :(List, 0).

flatlength([H|T], L) when is_list(H) ->
    :(H, :(T, L));
flatlength([_|T], L) ->
    :(T, L + 1);
flatlength([], L) -> L.

%% Internals

%% End of Module.
