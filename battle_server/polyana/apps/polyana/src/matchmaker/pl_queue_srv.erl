-module(pl_queue_srv).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, add_player/3, delete_player/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([find_game/1]). %убрать тест
-record(state, {dummy
            , match % убрать тест
}).
stop(Name) ->
    gen_server:call(Name, stop).

add_player(PlayerPid, PlayerId, {Currency, Bid}) ->
    gen_server:cast(?MODULE,
                    {add_player, PlayerPid, PlayerId, {Currency, Bid}}).

delete_player(PlayerPid) ->
    gen_server:cast(?MODULE, {delete_player, PlayerPid}).

%% убрать, начало для теста

find_game(PlayerSrv) ->
    gen_server:call(?MODULE, {find_game, PlayerSrv}).
%% конец для теста

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    lager:info("queue manager is starting"),
    ets:new(?MODULE, [named_table]),
    {ok, #state{dummy=1
            , match = #{} % убрать
    }}.

%% убрать, код для текста

handle_call({find_game, PlayerSrv}, _From, #state{match = Match}=State) ->
    {Reply, Match2} =  in_find_game(PlayerSrv, Match),
    {Reply1, Match3}= check_condition(Reply, Match2),
    State2 = State#state{match = Match3},
    {reply, Reply1, State2};

%% убрать

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_player, PlayerPid, PlayerId, {Currency, Bid}}, State) ->
    erlang:monitor(process, PlayerPid),
    {ok, Rating} = pl_storage_srv:get_rating(PlayerId),
    ets:insert(?MODULE, {PlayerPid, PlayerId, Rating, Currency, Bid}),
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



%%убрать, для теста

in_find_game(PlayerSrv, Match) ->
    case maps:find(PlayerSrv, Match) of
        {ok, _} -> {<<"Please, Waiting\n">>, Match};
        error -> Match2 = maps:put(PlayerSrv, none, Match),
            {<<"Searching Game\n">>, Match2}
    end.

check_condition(_Reply, Match) when map_size(Match) == 3 ->
    Players = maps:keys(Match),
    Match2 = lists:foldl(fun(E, Acc) -> maps:remove(E, Acc) end, Match, Players),
    {ok, Pid} = supervisor:start_child(meadow_battle_sup, [Players]),
%%  lists:foreach(fun(PlayerSrv) -> meadow_players_srv:add_battle_pid(PlayerSrv, Pid) end, Players),
%%  ok = meadow_battle:command(new, Pid),
    lager:info("new game started pid:~p", [Pid]),
    {{battle, <<"New Game started\n">>}, Match2};
check_condition(Reply, Match) ->
    {{ok, Reply}, Match}.


%% убрать, для теста