-module(dummy_client).

-export([run_client/4, test_parallel/3,
         test_client_crdt/1, test_client_crdt/2, test_client_crdt/3]).

-define(CATALOG,
        ordsets:from_list(
          ["Star Wars",
           "Blade Runner",
           "The Muppet Movie",
           "The Mask",
           "Star Trek",
           "Terminator",
           "The Abyss",
           "Alien",
           "Aliens",
           "Days of Heaven"])).

-define(MATRIX,
       [{1,   10},
        {1,   50},
        {1,   100},
        {1,   500},
        {1,   1000},
        {5,   10},
        {5,   20},
        {5,   100},
        {5,   200},
        {10,  1},
        {10,  10},
        {10,  50},
        {10,  100},
        {100, 1},
        {100, 10}]).

test_client_crdt(CMod) ->
    io:format("Results for: ~w~n~n", [CMod]),
    io:format("   Clients   Ops/client      Ops    Bytes~n"),
    test_client_crdt(?MATRIX, CMod).

test_client_crdt([], _) ->
    ok;
test_client_crdt([{Clients, Ops}|XS], CMod) ->
    {ok, Size} = test_client_crdt(Clients, Ops, CMod),
    io:format("  ~8w   ~10w ~8w ~8w~n",
              [Clients, Ops, Clients*Ops, Size]),
    test_client_crdt(XS, CMod).


test_client_crdt(Clients, Ops, CMod) ->
    {ok, _Server} = crdt_server:start_link(CMod),
    test_parallel(Clients, Ops, client_set),
    {ok, _, Size} = crdt_server:info(),
    ok = crdt_server:stop(),
    {ok, Size}.

test_parallel(Clients, Ops, Mod) ->
    Caller = self(),
    spawn_link(fun() ->
                       process_flag(trap_exit, true),
                       start_parallel(Clients, Ops, Mod),
                       await_clients(Clients),
                       Caller ! finished
               end),
    receive
        finished -> ok
    end.
    
start_parallel(0, _Ops, _Mod) ->
    ok;
start_parallel(C, Ops, Mod) ->
    spawn_link(?MODULE, run_client, [Ops, Mod, C, [C]]),
    start_parallel(C-1, Ops, Mod).

await_clients(0) ->
    ok;
await_clients(C) ->
    receive
        {'EXIT', _Pid, _Reason} -> await_clients(C-1)
    end.

run_client(N, Mod, ID, Args) ->
    {A1,A2,A3} = now(),
    random:seed(A1, erlang:crc32(<<ID>>) bxor A2, A3),
    {ok, StartQueue} = apply(Mod, connect, Args),
    client_loop(N, Mod, StartQueue).

client_loop(0, _Mod, _QueueC) ->
    %% io:format("[~w] final queue contents: ~p~n",
    %%          [self(), Mod:contents(QueueC)]),
    {finished};
client_loop(N, Mod, QueueC) ->
    %% io:format("[~w] queue: ~p~n", [self(), QueueC]),
    Queue = Mod:contents(QueueC),
    Action = user_action(Queue, ?CATALOG),
    %% io:format("[~w] action: ~p~n", [self(), Action]),
    {ok, NQueue} = Mod:update(Action, QueueC),
    % sleep a bit
    timer:sleep(10 + random:uniform(5)),
    client_loop(N-1, Mod, NQueue).


%% Simulates a user action.
%%
%% @spec user_action(Queue, Catalog) ->
%%                               {add, Movie} |
%%                               {remove, Movie}
user_action([], Catalog) ->
    %% empty queue, have to add something
    add_some_movie([], Catalog);
user_action(Queue, Catalog) ->
    %% non-empty queue, can add or remove
    case random:uniform(2) of
        1 -> add_some_movie(Queue, Catalog);
        2 -> remove_some_movie(Queue)
    end.

add_some_movie(OldQueue, CatList) ->
    case ordsets:subtract(CatList, ordsets:from_list(OldQueue)) of
        [] ->
            %% the entire catalog is already in the queue!
            remove_some_movie(OldQueue);
        Avail ->
            %% choose a random movie from those available
            Movie = lists:nth(random:uniform(ordsets:size(Avail)), Avail),
            {add, Movie}
    end.

remove_some_movie([]) ->
    {error, empty};
remove_some_movie(Queue) ->
    Victim = lists:nth(random:uniform(length(Queue)), Queue),
    {remove, Victim}.
        
