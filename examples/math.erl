%% Copyright © 2013 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- erlang coding: utf-8 -*-
-module(math).

%% math: some ubiquitous math functions.

-export([fac/1, fib/1]).

%% API

fac (0) -> 1;
fac (N) when N > 0, is_integer(N) ->
    N * fac(N -1).

fib (0) -> 0;
fib (1) -> 1;
fib (N) ->
    fib(N -1) + fib(N -2).

%% Internals

%% End of Module.
