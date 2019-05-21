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
            Players = match_players(Amount, Delta),

            if
                Amount == length(Players) ->
                    remove_from_queue(Players),
                    create_room(Players),
                    loop(State);

                true ->
                    NewDelta = Delta * Multiplier,

                    loop(#state{players_amount = Amount,
                                rating_delta = NewDelta,
                                expand_multiplier = Multiplier})
            end
    end.

match_players(Amount, Delta) ->
    PlayersAndRatings = ets:foldl(
        fun ({PlayerPid, _PlayerId, Rating}, Players) ->
            notify_player(PlayerPid),

            case length(Players) of
                Amount ->
                    Players;

                0 ->
                    [{PlayerPid, Rating}];

                _ ->
                    [{_Pid, EtalonRating} | _] = Players,

                    if
                        abs(EtalonRating - Rating) =< Delta ->
                            lists:append(Players, [{PlayerPid, Rating}]);

                        true ->
                            Players
                    end
            end
        end,
        [],
        pl_queue_srv
    ),

    {Players, _Ratings} = lists:unzip(PlayersAndRatings),
    Players.

notify_player(Pid) ->
    Pid ! matching_in_progress.

remove_from_queue(Players) ->
    lists:foreach(
        fun (PlayerPid) ->
            pl_queue_srv:delete_player(PlayerPid)
        end,
        Players
    ).

create_room(Players) ->
    lager:info("create room ~p", [Players]).
