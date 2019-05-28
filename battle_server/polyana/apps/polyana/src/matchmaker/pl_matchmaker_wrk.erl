-module(pl_matchmaker_wrk).

-export([start_link/2]).
-export([init/2]).

-record(state, {
    players_amount,
    rating_delta = 0.01,
    expand_multiplier = 2,
    battle_type
}).

start_link(PlayersAmount, BattleType) ->
    Pid = spawn_link(?MODULE, init, [PlayersAmount, BattleType]),
    {ok, Pid}.

init(PlayersAmount, BattleType) ->
    lager:info("Starting matching process ~p.", [BattleType]),

    State = #state{players_amount = PlayersAmount,
                   battle_type = BattleType},
    loop(State).

loop(#state{players_amount = Amount,
            rating_delta = Delta,
            expand_multiplier = Multiplier,
            battle_type = BattleType} = State) ->
    timer:sleep(2000),

    case ets:first(BattleType) of
        '$end_of_table' ->
            loop(State);

        _ ->
            {Currency,
             Bid,
             Players,
             PlayersCurrencies} = match_players(Amount, BattleType, Delta),

            if
                Amount == length(Players) ->
                    remove_from_queue(Players, BattleType),

                    create_room({Currency,
                                 Bid,
                                 lists:zip(Players, PlayersCurrencies)}),

                    loop(State);

                true ->
                    case Delta * Multiplier of
                        BigDelta when BigDelta > 1 ->
                            loop(State#state{rating_delta = 1}); % max delta

                        NewDelta ->
                            loop(State#state{rating_delta = NewDelta})
                    end

            end
    end.

match_players(Amount, BattleType, Delta) ->
    GetGameParams = fun (Player, Acc) ->
        get_game_parameters(Amount, Delta, Player, Acc)
    end,

    [{_PlayerPid, _PlayerId, _Rating, Currency, Bid}] =
        ets:lookup(BattleType, ets:first(BattleType)),

    {BattleCurrency, BattleBid, PlayersAndInfo} = ets:foldl(
        GetGameParams,
        {Currency, Bid, []},
        BattleType
    ),

    {Players, _Ratings, Currencies} = lists:unzip3(PlayersAndInfo),

    {BattleCurrency, BattleBid, Players, Currencies}.

get_game_parameters(Amount,
                    Delta,
                    {PlayerPid, _PlayerId, Rating, Currency, Bid},
                    {BattleCurrency, BattleBid, Players}) ->
    notify_player(PlayerPid),

    case length(Players) of
        Amount ->
            {BattleCurrency, BattleBid, Players};

        0 ->
            {Currency, Bid, [{PlayerPid, Rating, Currency}]};

        _ ->
            [{_Pid, EtalonRating, _PlayerCurrency} | _] = Players,

            if
                abs(EtalonRating - Rating) =< Delta ->
                    {BattleCurrency, MinBid} =
                        get_min_bid({BattleCurrency, BattleBid},
                                    {Currency, Bid}),

                    {
                        BattleCurrency,
                        MinBid,
                        lists:append(Players,
                                    [{PlayerPid, Rating, Currency}])
                    };

                true ->
                    {BattleCurrency, BattleBid, Players}
            end
    end.

get_min_bid({BattleCurrency, BattleBid}, {CurrencyType, Bid}) ->
    case BattleCurrency of
        CurrencyType ->
            {BattleCurrency, min(BattleBid, Bid)};

        _ ->
            BidInBattleCurrency =
                pl_storage_srv:exchange_currency(CurrencyType, BattleCurrency, Bid),

            {BattleCurrency, min(BattleBid, BidInBattleCurrency)}
    end.

notify_player(Pid) ->
    Pid ! matching_in_progress.

remove_from_queue(Players, BattleType) ->
    lists:foreach(
        fun (PlayerPid) ->
            pl_queue_srv:delete_player(PlayerPid, BattleType)
        end,
        Players
    ).

create_room(Parameters) ->
    lager:info("create room ~p", [Parameters]),
    {ok, _Pid} = supervisor:start_child(pl_battle_sup, [Parameters]).
