-module(dummy_client).

-export([run_client/2]).

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

run_client(Mod, N) ->
    {ok, StartQueue} = crdt_server:read(),
    client_loop(Mod, N, StartQueue).

client_loop(Mod, 0, QueueC) ->
    io:format("[~w] final queue contents: ~p~n",
             [self(), Mod:value(QueueC)]),
    {finished};
client_loop(Mod, N, QueueC) ->
    Queue = Mod:value(QueueC),
    Action = user_action(Queue, ?CATALOG),
    io:format("[~w] action: ~p~n", [self(), Action]),
    {ok, NQueue} = Mod:update(Action, self(), QueueC),
    Response = crdt_server:update(NQueue),
    io:format("[~w] response: ~p~n", [self(), Response]),
    NextState = case Response of
                    {ok, current} -> NQueue;
                    {ok, Updated} -> Updated
                end,
    % sleep 1 second
    timer:sleep(1000),
    client_loop(Mod, N-1, NextState).


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
        
