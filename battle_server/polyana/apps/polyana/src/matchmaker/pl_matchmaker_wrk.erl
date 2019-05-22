-module(pl_matchmaker_wrk).

-export([start_link/0]).
-export([init/0]).

-record(state, {
    players_amount = 2,
    rating_delta = 0.01,
    expand_multiplier = 2
}).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    lager:info("Starting matching process."),
    State = #state{},
    loop(State).

loop(#state{players_amount = Amount,
            rating_delta = Delta,
            expand_multiplier = Multiplier} = State) ->
    timer:sleep(2000),

    case ets:first(pl_queue_srv) of
        '$end_of_table' ->
            loop(State);

        _ ->
            {Currency,
             Bid,
             Players,
             PlayersCurrencies} = match_players(Amount, Delta),

            if
                Amount == length(Players) ->
                    remove_from_queue(Players),

                    create_room({Currency,
                                 Bid,
                                 lists:zip(Players, PlayersCurrencies)}),

                    loop(State);

                true ->
                    NewDelta = Delta * Multiplier,

                    loop(#state{players_amount = Amount,
                                rating_delta = NewDelta,
                                expand_multiplier = Multiplier})
            end
    end.

match_players(Amount, Delta) ->
    GetGameParams = fun (Player, Acc) ->
        get_game_parameters(Amount, Delta, Player, Acc)
    end,

    [{_PlayerPid, _PlayerId, _Rating, Currency, Bid}] =
        ets:lookup(pl_queue_srv, ets:first(pl_queue_srv)),

    {BattleCurrency, BattleBid, PlayersAndInfo} = ets:foldl(
        GetGameParams,
        {Currency, Bid, []},
        pl_queue_srv
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

get_min_bid({BattleCurrency, BattleBid}, {Currency, Bid}) ->
    lager:error("IMPLEMENT ME get_min_bid"),
    {BattleCurrency, 100}.

notify_player(Pid) ->
    Pid ! matching_in_progress.

remove_from_queue(Players) ->
    lists:foreach(
        fun (PlayerPid) ->
            pl_queue_srv:delete_player(PlayerPid)
        end,
        Players
    ).

create_room(Parameters) ->
    lager:info("create room ~p", [Parameters]).
