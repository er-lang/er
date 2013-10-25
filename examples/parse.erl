%% -*- erlang, coding: utf-8 -*-
-module(parse).
-compile({parse_transform,inline}).
-export([string/1]).

-spec string (Str::string()) -> {ok | error, term()}.
string (Str) ->
    case kju_internals:is_string(Str) of
        true ->
            {ok, Tokens} = xrl_scanner:string(Str),
            case yrl_parser:parse(Tokens) of
                {ok, Tree}=M0 -> M0;
                Error -> {error, Error} % MAY print stuff
            end
    end.
