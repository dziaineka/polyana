-module(pl_queue_srv).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, add_player/4, delete_player/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {dummy}).

stop(Name) ->
    gen_server:call(Name, stop).

add_player(PlayerPid, PlayerId, {Currency, Bid}, BattleType) ->
    gen_server:cast(?MODULE, {add_player,
                              PlayerPid,
                              PlayerId,
                              {Currency, Bid},
                              BattleType}).

delete_player(PlayerPid, BattleType) ->
    gen_server:cast(?MODULE, {delete_player, PlayerPid, BattleType}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    lager:info("queue manager is starting"),
    ets:new(head_to_head, [named_table]),
    ets:new(battle_royale, [named_table]),
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_player, PlayerPid, PlayerId, {Currency, Bid}, BattleType},
            State) ->
    erlang:monitor(process, PlayerPid),
    {ok, Rating} = pl_storage_srv:get_rating(PlayerId),
    true = ets:insert(BattleType, {PlayerPid, PlayerId, Rating, Currency, Bid}),
    {noreply, State};

handle_cast({delete_player, PlayerPid, BattleType}, State) ->
    true = ets:delete(BattleType, PlayerPid),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    lager:info("Player down said queue ~p ~p", [Pid, Info]),
    ets:delete(head_to_head, Pid),
    ets:delete(battle_royale, Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
