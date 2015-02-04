%% Copyright © 2013 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(non_tail_var).
-compile(export_all).
%% non_ta_tests: tests for module non_ta.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

f() ->
    M = true,
    M,
    false.

%% Internals

%% End of Module.
