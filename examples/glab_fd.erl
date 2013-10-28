%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc Implements (slightly?) unreliable failure detectors for the
%% async bully algorithm used in gl_async_bully.
%% @end
%%%-------------------------------------------------------------------
-module(glab_fd).

%% API
-export([new/1
         ,start/2
         ,stop/2
         ,filter_DOWN/2
         ,set/2
        ]).

-record(fd, {name :: atom(),
             nodes = orddict:new() :: orddict:orddict()
            }).

-opaque fd_set() :: #fd{}.

-export_type([ fd_set/0 ]).

%%====================================================================
%% API
%%====================================================================

-spec new(atom()) -> fd_set().
new(Name) when is_atom(Name) ->
    #fd{name=Name}.

-spec start(node(), fd_set()) -> fd_set().
start(Node, FD = #fd{name = Name, nodes = Nodes}) when is_atom(Node)->
    case orddict:is_key(Node, Nodes) of
        true ->
            FD;
        false ->
            Ref = erlang:monitor(process, {Name, Node}),
            FD#fd{nodes=orddict:store(Node, Ref, Nodes)}
    end.

-spec filter_DOWN({'DOWN', reference(), _, _, Info}, fd_set()) ->
                         {'down', {atom(), node(), Info}, fd_set()} |
                         {'ignore', Reason::atom(), fd_set()}.
filter_DOWN({'DOWN', Ref, process, {Name, Node}, Info},
            FD = #fd{name = Name, nodes = Nodes}) ->
    case orddict:find(Node, Nodes) of
        {ok, Ref} ->
            {down, {Name, Node, Info},
             FD#fd{nodes=orddict:erase(Node, Nodes)}};
        {ok, WrongRef} when is_reference(WrongRef) ->
            {ignore, stale, FD};
        error ->
            {ignore, not_monitored, FD}
    end;
filter_DOWN({'DOWN', _Ref, _Type, _Obj, _Info}, FD) ->
    {ignore, other_montior, FD}.


-spec stop(node(), fd_set()) -> fd_set().
stop(Node, FD = #fd{nodes = Nodes}) when is_atom(Node) ->
    case orddict:find(Node, Nodes) of
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]),
            FD#fd{nodes=orddict:erase(Node, Nodes)};
        error ->
            FD
    end.

-spec set([node()], fd_set()) -> fd_set().
set(NewNodes, FD = #fd{nodes = Nodes}) ->
    lists:foldl(fun start/2,
                lists:foldl(fun stop/2, FD,
                            orddict:fetch_keys(Nodes)),
                NewNodes).

%%====================================================================
%% Internal functions
%%====================================================================
