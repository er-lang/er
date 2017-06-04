%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(zero).

%% zero: 

-export([ zero/0
        , zero_/0 ]).


%% API

id (X) -> X.

'°' (F, G) ->
    fun (X) -> F(G(X)) end.

zero () ->
    F = fun (X) -> X - 1 end,
    G = fun (X) -> X + 0 end,
    H = '°'(G, '°'(F, fun id/1)),
    H(1).

zero_ () -> 0.

%% Internals

%% End of Module.
