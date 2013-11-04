-module(server_set).

-export([connect/0, fetch/1, contents/1, add/2, remove/2, update/2]).
-export_type([sset/0]).

-opaque sset() :: {[term()], binary()}.

-spec connect() -> {ok, sset()}.
-spec fetch(sset()) -> {ok, sset()}.
-spec contents(sset()) -> [term()].
-spec add(term(), sset()) -> {ok, sset()}.
-spec remove(term(), sset()) -> {ok, sset()}.
-spec update({add, term()}
             | {remove, term()},
             sset()) -> {ok, sset()}.

connect() ->
    {ok, Vals, Ctx} = set_server:read(),
    {Vals, Ctx}.

fetch(_) ->
    {ok, Vals, Ctx} = set_server:read(),
    {ok, {Vals, Ctx}}.

contents({Vals, _Ctx}) ->
    Vals.

add(Val, _) ->
    {ok, Vals, Ctx} = set_server:add(Val),
    {ok, {Vals, Ctx}}.

remove(Val, {_, Ctx}) ->
    {ok, NVals, NCtx} = set_server:remove(Val, Ctx),
    {ok, {NVals, NCtx}}.

update({add, Val}, S) ->
    add(Val, S);
update({remove, Val}, S) ->
    remove(Val, S).
