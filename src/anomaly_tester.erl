-module(anomaly_tester).

-export([start_test/1, start_test/2, run_client/3]).

start_test(NItems) ->
    start_test(NItems, {0.0, 1.0, 0}).

start_test(NItems, EParams) ->
    spawn_link(fun() -> run_test_proc(16, NItems, EParams) end).

run_test_proc(NClients, NItems, EParams) ->
    {ok, _Server} = set_server:start_link(riak_dt_orswot, 1, EParams),
    case run_test(NClients, NItems) of
        ok ->
            io:format("Test succeeded.~n");
        {error, EType, Info} ->
            io:format("Test failed: ~w: ~w~n", [EType, Info])
    end,
    set_server:stop().

run_test(NClients, NItems) ->
    Clients = start_clients(NClients, NItems),
    await_clients(Clients, NClients),
    {ok, Res} = collect_results(Clients, []),
    validate_results(Res).

await_clients(Clients, 0) ->
    ok = await_empty(),
    io:format("server set empty.~n"),
    lists:foreach(fun({Client, _Q}) -> Client ! shutdown end,
                  Clients),
    io:format("sent shutdown message to all clients.~n"),
    ok;
await_clients(Clients, Active) ->
    receive
        {queue_empty, Client} ->
            io:format("[~w] queue empty, ~w active.~n",
                      [Client, Active-1]),
            await_clients(Clients, Active-1)
    end.

await_empty() ->
    case set_server:item_count() of
        0 ->
            ok;
        _N ->
            %% io:format("server set not yet empty: ~p~n",
            %%          [Items]),
            timer:sleep(50),
            await_empty()
    end.
            

collect_results([], Acc) ->
    {ok, Acc};
collect_results(Pending, Acc) ->
    receive
        {results, Client, Rem} ->
            {value, {Client, Queue}, P2} = lists:keytake(Client, 1, Pending),
            collect_results(P2, [{Client, Queue, Rem}|Acc])
    end.

validate_results(Res) ->
    %% Each client should have added a disjoint set of elements.
    %% Each client should have removed another disjoint set of elements.
    %% And the server set should be empty.
    case server_set:connect() of
        {ok, SSet} ->
            case server_set:contents(SSet) of
                [] ->
                    validate_results(Res, ordsets:new(), ordsets:new(), 0);
                Contents ->
                    {error, server_remaining, Contents}
            end
    end.

validate_results([], Added, Removed, NDups) ->
    0 = ordsets:size(ordsets:subtract(Added, Removed)),
    0 = ordsets:size(ordsets:subtract(Removed, Added)),
    case NDups of
        0 ->
            ok;
        N ->
            io:format("Total duplicates: ~w~n", [N]),
            {error, duplicates_removed, N}
    end;
validate_results([{Client, CAdded, CRem}|XS], Added, Removed, Dups) ->
    {ok, CAddedSet} = unique_set(CAdded),
    {CRemSet, NDups} =
        case unique_set(CRem) of
            {ok, S} ->
                {S, Dups};
            {dups, S, CDups} ->
                io:format("Client ~w removed ~w duplicates!",
                          [Client, CDups]),
                {S, Dups+CDups}
        end,
    true = ordsets:is_disjoint(CAddedSet, Added),
    true = ordsets:is_disjoint(CRemSet, Removed),
    validate_results(XS,
                     ordsets:union(CAddedSet, Added),
                     ordsets:union(CRemSet, Removed),
                     NDups).


start_clients(N, ItemCount) ->
    [start_client(ID, ItemCount) || ID <- lists:seq(0, N-1)].

start_client(ID, Count) ->
    {A1,A2,A3} = now(),
    CRC = erlang:crc32(<<ID>>),
    Seed = {CRC bxor A1, CRC bxor A2, CRC bxor A3},
    Queue = gen_queue(ID, Count),
    Client = spawn_link(?MODULE, run_client,
                        [Queue, {ID, self()}, Seed]),
    io:format("[~w] started client.~n", [Client]),
    {Client, Queue}.

gen_queue(ID, Count) ->
    Start = ID*4096,
    randomize_list(lists:seq(Start, Start+Count)).

run_client(Queue, Ctx, Seed) ->
    random:seed(Seed),
    {ok, SSet} = server_set:connect(),
    client_pending(Queue, [], SSet, Ctx).

client_pending([], Removed, SSet, Ctx={_ID, Ctl}) ->
    Ctl ! {queue_empty, self()},
    client_watching(Removed, SSet, Ctx);
client_pending(Queue=[X|XS], Removed, SSet, Ctx) ->
    timer:sleep(10 + random:uniform(5)),
    RemC = remove_candidate(SSet, Ctx),
    case {RemC, random:uniform()} of
        {{found, Elt}, R} when R < 0.5 ->
            {ok, SS2} = server_set:remove(Elt, SSet),
            client_pending(Queue, [Elt|Removed], SS2, Ctx);
        _ ->
            {ok, SS2} = server_set:add(X, SSet),
            client_pending(XS, Removed, SS2, Ctx)
    end.

client_watching(Removed, SSet, Ctx={_ID, Ctl}) ->
    case remove_candidate(SSet, Ctx) of
        {found, Elt} ->
            {ok, SS2} = server_set:remove(Elt, SSet),
            client_watching([Elt|Removed], SS2, Ctx);
        notfound ->
            receive
                shutdown ->
                    Ctl ! {results, self(), Removed},
                    ok
            after 10 ->
                    {ok, SS2} = server_set:fetch(SSet),
                    client_watching(Removed, SS2, Ctx)
            end
    end.

remove_candidate(SSet, {ID, _Ctl}) ->
    %% io:format("[~w]: looking for candidate in: ~p~n",
    %%           [ID, server_set:contents(SSet)]),
    pred_find(server_set:contents(SSet),
              fun(Val) -> (Val rem 16) =:= ID end).

pred_find([], _Pred) ->
    notfound;
pred_find([X|XS], Pred) ->
    case Pred(X) of
        true  -> {found, X};
        false -> pred_find(XS, Pred)
    end.

randomize_list(List) ->
    Shuffled = lists:keysort(2, [{X, random:uniform()} || X <- List]),
    [X || {X, _K} <- Shuffled].

unique_set(List) ->
    unique_set_from(lists:sort(List), [], 0).

%% TODO
unique_set_from([], Acc, 0) ->
    {ok, lists:reverse(Acc)};
unique_set_from([], Acc, N) ->
    {dups, lists:reverse(Acc), N};
unique_set_from([X|XS], Acc=[X|_], Dups) ->
    unique_set_from(XS, Acc, Dups+1);
unique_set_from([X|XS], Acc, Dups) ->
    unique_set_from(XS, [X|Acc], Dups).

