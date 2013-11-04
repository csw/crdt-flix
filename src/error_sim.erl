-module(error_sim).

-export([wrap_call/2]).
-export([handle_call/6]).

%% Client side

wrap_call(Server, Request) ->
    case gen_server:call(Server, Request) of
        {success, Reply} ->
            Reply;
        {network_error, RetryDelay} ->
            timer:sleep(RetryDelay),
            wrap_call(Server, Request)
    end.

%% Server side

handle_call(Req, _From, State, Mod, Fun, {PErr, PExec, RetryDelay}) ->
    case random:uniform() < PErr of
        true -> %% simulate a network error
            {reply,
             {network_error, RetryDelay},
             case random:uniform() < PExec of
                 true ->  %% service the request anyway, drop the real reply
                     {reply, _Reply, NState} = Mod:Fun(Req, _From, State),
                     NState;
                 false -> %% don't service the request
                     State
             end};
        false ->
            Mod:Fun(Req, _From, State)
    end.
