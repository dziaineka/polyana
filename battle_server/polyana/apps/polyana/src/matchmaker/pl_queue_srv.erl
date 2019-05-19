-module(pl_queue_srv).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, add_player/2, delete_player/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {dummy}).
stop(Name) ->
    gen_server:call(Name, stop).

add_player(PlayerPid, PlayerId) ->
    gen_server:cast(?MODULE, {add_player, PlayerPid, PlayerId}).

delete_player(PlayerPid) ->
    gen_server:cast(?MODULE, {delete_player, PlayerPid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    lager:info("queue manager is starting"),
    ets:new(?MODULE, [named_table]),
    {ok, #state{dummy=1}}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_player, PlayerPid, PlayerId}, State) ->
    erlang:monitor(process, PlayerPid),
    {ok, Rating} = pl_storage_srv:get_rating(PlayerId),
    ets:insert(?MODULE, {PlayerPid, PlayerId, Rating}),
    {noreply, State};

handle_cast({delete_player, PlayerPid}, State) ->
    ets:delete(?MODULE, PlayerPid),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    lager:info("Player down ~p ~p", [Pid, Info]),
    ets:delete(?MODULE, Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
