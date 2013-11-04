%%%-------------------------------------------------------------------
%%% @author Clayton Wheeler <cswh@umich.edu>
%%% @copyright (C) 2013, Clayton Wheeler
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by Clayton Wheeler <cswh@umich.edu>
%%%-------------------------------------------------------------------
-module(set_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([read/0, add/1, remove/2]).
-export([size/0]).

%% Private interface for error_sim
-export([service_call/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec read() -> {ok, [term()], binary()}.
read() ->
    error_sim:wrap_call(?MODULE, {read}).

-spec add(term()) -> {ok, term()}.
add(Value) ->
    error_sim:wrap_call(?MODULE, {add, Value}).

remove(Value, Context) ->
    error_sim:wrap_call(?MODULE, {remove, Value, Context}).

size() ->
    gen_server:call(?MODULE, {size}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({Mod, ID}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Mod, ID}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Mod, ID, EParams}) ->
    {ok, {Mod, ID, Mod:new(), EParams}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({size}, _From, State={_Mod, _ID, Set, _EParams}) ->
    {reply, length(term_to_binary(Set)), State};
handle_call(Req, From, State={_Mod, _ID, _Set, EParams}) ->
    error_sim:handle_call(Req, From, State,
                          set_server, service_call, EParams).

service_call({read}, _From, State={Mod, _ID, Set, _EParams}) ->
    Reply = {ok, Mod:value(Set), term_to_binary(Set)},
    {reply, Reply, State};
service_call({add, Val}, _From, {Mod, ID, Set, EParams}) ->
    {ok, SetU} = Mod:update({add, Val}, ID, Set),
    Reply = {ok, Mod:value(SetU), term_to_binary(SetU)},
    {reply, Reply, {Mod, ID, SetU, EParams}};
service_call({remove, Val, Ctx}, _From, {Mod, ID, Set, EParams}) ->
    {ok, SetRm} = Mod:update({remove, Val},
                             binary_to_term(Ctx)),
    SetM = Mod:merge(Set, SetRm),
    Reply = {ok, Mod:value(SetM), term_to_binary(SetM)},
    {reply, Reply, {Mod, ID, SetM, EParams}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
