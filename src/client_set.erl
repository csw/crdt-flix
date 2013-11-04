-module(client_set).

-export([connect/1, fetch/1, contents/1, add/2, remove/2, update/2]).

-opaque cset() :: {module(), term(), tuple()}.

-spec connect(term()) -> {ok, cset()}.
-spec fetch(cset()) -> {ok, cset()}.
-spec contents(cset()) -> [term()].
-spec add(term(), cset()) -> {ok, cset()}.
-spec remove(term(), cset()) -> {ok, cset()}.
-spec update({add, term()}
             | {remove, term()},
             cset()) -> {ok, cset()}.

connect(Actor) ->
    {ok, CMod, _} = crdt_server:info(),
    fetch({CMod, Actor, false}).

fetch({CMod, Actor, _}) ->
    {ok, CRDT} = crdt_server:read(),
    {ok, {CMod, Actor, CRDT}}.

contents({CMod, _, CRDT}) ->
    CMod:value(CRDT).

add(Val, CSet) ->
    do_update({add, Val}, CSet).

remove(Val, CSet) ->
    do_update({remove, Val}, CSet).

update(U={add, _}, CSet) ->
    do_update(U, CSet);
update(U={remove, _}, CSet) ->
    do_update(U, CSet).


do_update(Update, {CMod, Actor, CRDT}) ->
    {ok, C2} = CMod:update(Update, Actor, CRDT),
    case crdt_server:update(C2) of
        {ok, current} -> {ok, {CMod, Actor, C2}};
        {ok, C3}      -> {ok, {CMod, Actor, C3}}
    end.
    
