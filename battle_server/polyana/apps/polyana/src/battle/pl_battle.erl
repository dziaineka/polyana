%%%-------------------------------------------------------------------
%%% @author kalinin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Май 2019 13:44
%%%-------------------------------------------------------------------
-module(pl_battle).
-author("kalinin").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([stop/1, move/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    battle_id,
    players_info,
    battle_field,
    battle_field_size,
    turn_order,
    bid,
    currency_type,
    winner = undefined,
    turn_count = 0,
    round,
    fire
}).

-record(player_info, {
    position,
    mark,
    currency_type,
    id
}).

-record(round, {
    count,
    status
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(Players) ->
    gen_server:start_link(?MODULE, Players, []).

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

init({CurrencyType, Bid, PlayersWithCurrencies}) ->
    {Players, Currencies} = lists:unzip(PlayersWithCurrencies),
    {ok, FieldHeight} = application:get_env(polyana, battle_field_size),
    {ok, FieldWidth} = application:get_env(polyana, battle_field_size),

    send_battle_pid(Players),
    monitor_players(Players),
    Order = shuffle(Players),
    PlayersInfo = get_players_info(Players, Currencies),
    Field = create_field(PlayersInfo),
    StringField = field_to_msg(Field, {FieldHeight, FieldWidth}),

    {ok, BattleId} = save_start_game_data(CurrencyType,
                                          Bid,
                                          PlayersWithCurrencies),

    invite_players_to_game(PlayersInfo, Order, StringField),
    Turn_Count = length(Order),
    Round = #round{count = 1, status = inactive},

    State = #state{battle_id = BattleId,
                   players_info = PlayersInfo,
                   battle_field = Field,
                   battle_field_size = {FieldHeight, FieldWidth},
                   turn_order = Order,
                   bid = Bid,
                   currency_type = CurrencyType,
                    turn_count = Turn_Count,
                    round = Round,
                    fire = #{}
        },

    {ok, State}.

send_battle_pid(Players) ->
    lists:foreach(
        fun(PlayerPid) ->
            pl_player_srv:add_battle_pid(PlayerPid, self())
        end,
        Players
    ).

monitor_players(Players) ->
    lists:foreach(
        fun(PlayerPid) ->
            monitor(process, PlayerPid)
        end,
        Players
    ).

create_field(PlayersInfo) ->
    {ok, FieldHeight} = application:get_env(polyana, battle_field_size),
    {ok, FieldWidth} = application:get_env(polyana, battle_field_size),

    maps:fold(
        fun (_PlayerPid, #player_info{position = Position, mark = Mark}, Acc) ->
            maps:update(Position, Mark, Acc)
        end,
        gen_empty_field(FieldHeight, FieldWidth),
        PlayersInfo
    ).

move(BattlePid, PlayerPid, Direction) ->
    gen_server:cast(BattlePid, {move, PlayerPid, Direction}).


stop(BattleSrv)->
    lager:info("stop room ~p", [BattleSrv]),
    gen_server:cast(BattleSrv, stop).

multicast(Reply, Players) ->
    case Players of
        {Active, PlayersList} ->
            Active ! {message, <<Reply/binary, "It's your turn", "\n">>},
            multicast(Reply, lists:delete(Active, PlayersList));

        PlayersList ->
            lists:foreach(
                fun(PlayerSrv) ->
                    PlayerSrv ! {message, Reply}
                end,
                PlayersList
            )
    end.

save_start_game_data(CurrencyType, Bid, PlayersWithCurrencies) ->
    {Players, _PlayersCurrencies} = lists:unzip(PlayersWithCurrencies),

    % записываем в базу игру
    {ok, BattleId} = pl_storage_srv:save_battle(
        CurrencyType,
        Bid,
        lists:map(fun pl_player_srv:get_id/1, Players)),

    lists:foreach(
        fun ({PlayerPid, PlayerCurrencyType}) ->
            PlayerId = pl_player_srv:get_id(PlayerPid),

            % добавляем игроку одну игру
            pl_storage_srv:add_played_game(PlayerId),
            % рассчитываем новый винрейт
            pl_storage_srv:update_winrate(PlayerId),

            % записываем событие о начале игры в лог
            {ok, EventId} = pl_storage_srv:save_event(battle_start,
                                                      BattleId,
                                                      PlayerId),

            % снимаем с игрока деньги
            pl_storage_srv:save_transaction(
                EventId,
                PlayerId,
                PlayerCurrencyType,
                pl_storage_srv:exchange_currency(CurrencyType,
                                                 PlayerCurrencyType,
                                                 Bid) * -1)
        end,
        PlayersWithCurrencies
    ),

    {ok, BattleId}.

invite_players_to_game(PlayersInfo, [First|_Order], StringField) ->
    Players = maps:keys(PlayersInfo),
    Field = list_to_binary(StringField),

    lists:foreach(
        fun (PlayerPid) ->
            #player_info{mark = Mark} = maps:get(First, PlayersInfo),

            multicast(<<Field/binary, "Now turn player ", Mark/binary, "\n">>,
                      [PlayerPid])
        end,
        Players
    ),

    multicast(<<"It's your turn", "\n">>, [First]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({move, PlayerPid, Direction},
            #state{battle_field = Field,
                battle_field_size = Size,
                players_info = PlayersInfo,
                turn_order = Order,
                turn_count = Turn_Count,
                round = Round,
                fire = Fire} = State) ->
    {Flag, Msg, NewField, NewPlayersInfo, NewOrder, Turn_Count2, New_Round} =
        move(PlayerPid, Direction, Field, PlayersInfo, Order, Turn_Count, Round),

    case Flag of
        nok ->
            StringField = field_to_msg(Field, Size),
            Reply = <<(list_to_binary(StringField))/binary, Msg/binary, "\n">>,
            multicast(Reply, [NewPlayersInfo]),
            {noreply, State};

        single ->
            [First| _Others] = Order,
            StringField = field_to_msg(Field, Size),
            Reply = <<(list_to_binary(StringField))/binary, Msg/binary, "\n">>,
            multicast(Reply, [First]),
            {noreply, State};

        multi ->
            check_battle_conditions(NewField, NewPlayersInfo, NewOrder,
                                    Size, State, Msg, Turn_Count2, New_Round, Fire)
    end;

handle_cast(stop, #state{battle_id = BattleId,
                         players_info = PlayersInfo,
                         currency_type = CurrencyType,
                         bid = Bid,
                         winner = WinnerPid} = State) ->
    save_end_game_data(BattleId,
                       WinnerPid,
                       PlayersInfo,
                       CurrencyType,
                       Bid),

    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'DOWN', _Ref, process, PlayerPid, Info},
            #state{battle_field = BattleField,
                   turn_order = TurnOrder,
                   players_info = PlayersInfo,
                   battle_field_size = Size,
                   turn_count = Turn_Count,
                   round = Round,
                   fire = Fire} = State) ->
    lager:info("Player down, he is lost ~p ~p", [PlayerPid, Info]),
    #player_info{mark = Mark} = maps:get(PlayerPid, PlayersInfo),

    Msg = <<"Player ",Mark/binary, " is gone.\n">>,

    check_battle_conditions(BattleField, PlayersInfo, TurnOrder,
                            Size, State, Msg, Turn_Count, Round, Fire),

    {noreply, State#state{turn_order = lists:delete(PlayerPid, TurnOrder)}};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_battle_conditions(NewField, NewPlayersInfo, NewOrder, Size, State, Msg,
    Turn_Count2, New_Round, Fire) ->
    {NewMsg, NewField2, Round2, Fire2} =
        set_fire(Msg, NewField, New_Round, Fire),

    {Order3, Turn_Count3} = lose_condition(NewField2,
                                           NewPlayersInfo,
                                           NewOrder,
                                           NewOrder,
                                           Turn_Count2),

    StringField = field_to_msg(NewField2, Size),
    NewPlayersList = maps:keys(NewPlayersInfo),

    case win_condition(Order3, NewPlayersInfo) of
        next ->
            State2 = State#state{battle_field = NewField2,
                                players_info = NewPlayersInfo,
                                turn_order = Order3,
                                turn_count = Turn_Count3,
                                round = Round2,
                                fire = Fire2},

            [NextPlayer | _] = Order3,

            #player_info{mark = Mark} =
                maps:get(NextPlayer, NewPlayersInfo),

            Msg2 = <<NewMsg/binary, " player ", Mark/binary>>,
            Reply = <<(list_to_binary(StringField))/binary, Msg2/binary, "\n">>,
            multicast(Reply, {NextPlayer, NewPlayersList}),
            {noreply, State2};

        {WinnerPid, WinMessage} ->
            State2 = State#state{battle_field = NewField,
                                players_info = NewPlayersInfo,
                                turn_order = Order3,
                                winner = WinnerPid},

            Reply = <<(list_to_binary(StringField))/binary,
                      WinMessage/binary, "\n">>,

            multicast(Reply, NewPlayersList),

            stop(self()),

            {noreply, State2}
    end.

move(PlayerPid, Direction, Field, PlayersInfo,
                [Active|_Passive] = Order, Turn_Count,
                Round) when Active == PlayerPid ->
    PlayerInfo = maps:get(Active, PlayersInfo),
    {Y, X} = PlayerInfo#player_info.position,

    case Direction of
        up ->
            in_move(Field, {Y - 1, X}, PlayersInfo, Order, Turn_Count, Round);

        down ->
            in_move(Field, {Y + 1, X}, PlayersInfo, Order, Turn_Count, Round);

        left ->
            in_move(Field, {Y, X - 1}, PlayersInfo, Order, Turn_Count, Round);

        right ->
            in_move(Field, {Y, X + 1}, PlayersInfo, Order, Turn_Count, Round)
    end;

move(PlayerPid, _Direction, Field, _PlayersPos,
            [Active|_Passive]=Order, Turn_Count,
            Round) when Active=/= PlayerPid ->
    {nok, <<"Not your turn">>, Field, PlayerPid, Order, Turn_Count, Round}.


in_move(Field, NewPosition, PlayersInfo,
        [Active | Passive], Turn_Count, Round) ->
    PlayerInfo = maps:get(Active, PlayersInfo),

    case maps:find(NewPosition, Field) of
        {ok, <<"O">>} ->
            Field2 = Field#{PlayerInfo#player_info.position => <<"X">>,
                            NewPosition => PlayerInfo#player_info.mark},

            NewPlayersInfo = maps:update(
                Active,
                PlayerInfo#player_info{position = NewPosition},
                PlayersInfo),

            {Order, New_Turn_Count, New_Round} =
                change_order([Active | Passive], Turn_Count, Round),

            {
                multi,
                <<"Next turn">>,
                Field2,
                NewPlayersInfo,
                Order,
                New_Turn_Count,
                New_Round
            };

        _ ->
            {
                single,
                <<"NO WAY">>,
                Field,
                [Active],
                [Active | Passive],
                Turn_Count,
                Round
            }
    end.


gen_empty_field(FieldHeight, FieldWidth) ->
    gen_empty_field(FieldHeight, FieldWidth, #{}, FieldHeight, FieldWidth).


gen_empty_field(_M, -1, Acc, _M_orig, _N_orig) ->
    Acc;

gen_empty_field(-1, FieldWidth, Acc, FieldHeight_orig, FieldWidth_orig) ->
    gen_empty_field(FieldHeight_orig, FieldWidth-1, Acc,
                    FieldHeight_orig, FieldWidth_orig);

gen_empty_field(FieldHeight, FieldWidth, Acc,
                FieldHeight_orig, FieldWidth_orig) ->
    case rand:uniform(8) of
        1 ->
            Acc2 = Acc#{{FieldHeight,FieldWidth} => <<"X">>};

        _ ->
            Acc2 = Acc#{{FieldHeight,FieldWidth} => <<"O">>}
    end,

    gen_empty_field(FieldHeight-1, FieldWidth, Acc2,
                    FieldHeight_orig, FieldWidth_orig).


change_order([Active|Passive], 1, Round)->
    New_Round = check_round(Round),
    Order = lists:append(Passive, [Active]),
    Turn = length(Order),
    [Active1|Passive2] =Order,                  % две строчки кода, для того, чтобы
    Order2 = lists:append(Passive2, [Active1]), % менять первого игрока в начале раунда
    {Order2, Turn, New_Round};
change_order([Active|Passive], Turn_Count, Round)->
    Order = lists:append(Passive, [Active]),
    {Order, Turn_Count-1, Round}.


lose_condition(_Field, _PlayersInfo, _, Order, Turn_Count)
            when length(Order) == 1 ->
    {Order, Turn_Count};

lose_condition(_Field, _PlayersInfo, [], Order, Turn_Count) ->
    {Order, Turn_Count};

lose_condition(Field, PlayersInfo, [Active|Others], Order, Turn_Count)->
    #player_info{position = Position, mark = Mark} =
        maps:get(Active, PlayersInfo),

    Directions = [up, down, left, right],

    case find_direction(Field, Position, Directions) of
        ok ->
            lose_condition(Field, PlayersInfo, Others, Order, Turn_Count);

        lose ->
            lager:info("Player ~p lose", [Mark]),
            Order2 = lists:delete(Active, Order),
            multicast(<<"You lose!\n">>, [Active]),
            lose_condition(Field, PlayersInfo, Others, Order2, Turn_Count-1)
    end.


win_condition(Order, PlayersInfo) when length(Order) == 1 ->
    [PlayerPid] = Order,
    #player_info{mark = Mark} = maps:get(PlayerPid, PlayersInfo),
    multicast(<<"You win!\n">>, [PlayerPid]),
    {PlayerPid, <<Mark/binary, " wins. Congratulations!">>};

win_condition(_Order, _PlayersInfo) ->
    next.


find_direction(_Field, {_Y,_X}, []) ->
    lose;

find_direction(Field, {Y, X}, [up|Directions]) ->
    case maps:find({Y - 1, X}, Field) of
        {ok, <<"O">>} ->
            ok;

        {ok,_} ->
            find_direction(Field, {Y, X}, Directions);

        error ->
            find_direction(Field, {Y, X}, Directions)
    end;

find_direction(Field, {Y, X}, [down|Directions]) ->
    case maps:find({Y + 1, X}, Field) of
        {ok, <<"O">>} ->
            ok;

        {ok,_} ->
            find_direction(Field, {Y, X}, Directions);

        error ->
            find_direction(Field, {Y, X}, Directions)
    end;

find_direction(Field, {Y, X}, [left | Directions])->
    case maps:find({Y, X - 1}, Field) of
        {ok, <<"O">>} ->
            ok;

        {ok, _} ->
            find_direction(Field, {Y, X}, Directions);

        error ->
            find_direction(Field, {Y, X}, Directions)
    end;

find_direction(Field, {Y, X}, [right|Directions]) ->
    case maps:find({Y, X + 1}, Field) of
        {ok, <<"O">>} ->
            ok;

        {ok, _} ->
            find_direction(Field, {Y, X}, Directions);

        error ->
            find_direction(Field, {Y, X}, Directions)
    end.


field_to_msg(Field, {SizeFromZero, _})->
    {_Positions, CellValues} =
            lists:unzip(lists:keysort(1, maps:to_list(Field))),

    StringField = draw_field(CellValues, SizeFromZero + 1),
    ["\n", StringField, "\n\n"].

draw_field(Cells, Size) ->
    draw_field(Cells, Size, []).


draw_field(_Cells, Size, Field) when length(Field) >= Size ->
    [lists:reverse(Field), "\n" | draw_row_top(lists:seq(1, Size))];

draw_field(Cells, Size, Field) ->
    StartPos = 1 + Size * length(Field),
    Row = lists:sublist(Cells, StartPos, Size),
    draw_field(Cells, Size, [draw_row(Row) | Field]).


draw_row(Row) ->
    lists:flatten(["\n", draw_row_top(Row), "\n", draw_row_bottom(Row), "|"]).

draw_row_top(Row) ->
    lists:foldl(
        fun
            (_, RowTop) ->
                [" ---" | RowTop]
        end,
        [],
        Row
    ).

draw_row_bottom(Row) ->
    lists:foldl(
        fun
            (<<"O">>, RowTop) ->
                ["|   " | RowTop];

            (<<"X">>, RowTop) ->
                ["| X " | RowTop];

            (Symbol, RowTop) ->
                ["| " ++ binary_to_list(Symbol) ++ " " | RowTop]
        end,
        [],
        lists:reverse(Row)
    ).

shuffle(List)->
    Random_list = [{rand:uniform(), X} || X <- List],
    [X || {_, X}<- lists:sort(Random_list)].


get_players_info(Players, Currencies) ->
    get_players_info(Players, Currencies, #{}).

get_players_info([], _Currencies, PlayersInfo) ->
    PlayersInfo;

get_players_info([PlayerPid | Players],
                 [Currency | Currencies],
                 PlayersInfo) ->
    PlayerInfo = #player_info{
                        currency_type = Currency,
                        position = get_initial_field_pos(length(Players) + 1),
                        mark = get_mark(length(Players) + 1),
                        id = pl_player_srv:get_id(PlayerPid)},

    get_players_info(Players,
                     Currencies,
                     PlayersInfo#{PlayerPid => PlayerInfo}).

get_initial_field_pos(Number) ->
    {ok, FieldHeight} = application:get_env(polyana, battle_field_size),
    {ok, FieldWidth} = application:get_env(polyana, battle_field_size),

    Positions = [{0, 0},
                 {FieldHeight, FieldWidth},
                 {FieldHeight, 0},
                 {0, FieldWidth}],

    lists:nth(Number, Positions).

get_mark(Number) ->
    Marks = [<<"A">>, <<"B">>, <<"C">>, <<"D">>],
    lists:nth(Number, Marks).

save_end_game_data(BattleId, WinnerPid, PlayersInfo, CurrencyType, Bid) ->
    lists:foreach(
        fun ({PlayerPid, #player_info{currency_type = PlayerCurrencyType,
                                       id = PlayerId}}) ->
            % сохраним событие о конце игры
            {ok, EventId} = pl_storage_srv:save_event(battle_end,
                                                      BattleId,
                                                      PlayerId),
            ok = award_achievements(PlayerId, PlayerPid),

            case (WinnerPid == PlayerPid) of
                true ->
                    % сохраняем игрока как победителя игры
                    ok = pl_storage_srv:save_winner(BattleId, PlayerId),
                    % добавим единичку к выигранным
                    pl_storage_srv:add_won_game(PlayerId),
                    % пересчитаем винрейт
                    pl_storage_srv:update_winrate(PlayerId),

                    % переведем победителю банк
                    pl_storage_srv:save_transaction(
                        EventId,
                        PlayerId,
                        PlayerCurrencyType,
                        pl_storage_srv:exchange_currency(
                                        CurrencyType,
                                        PlayerCurrencyType,
                                        Bid) * maps:size(PlayersInfo));

                _ ->
                    ok
            end
        end,
        maps:to_list(PlayersInfo)
    ).

award_achievements(PlayerId, PlayerPid) ->
    award_5_battles(PlayerId, PlayerPid),
    award_5_wins(PlayerId, PlayerPid),
    award_first_win(PlayerId, PlayerPid).

award_5_battles(PlayerId, PlayerPid) ->
    {ok, BattlesAmount} = pl_storage_srv:get_played_battles(PlayerId),
    AchievementAlreadyReceived =
        pl_storage_srv:check_achievement_ownership(PlayerId, '5_battles'),

    case {BattlesAmount, AchievementAlreadyReceived} of
        {_, true} ->
            ok;

        {BattlesAmount, false} when BattlesAmount >= 5 ->
            {ok, AchievementId} =
                pl_storage_srv:save_achievement(PlayerId, '5_battles'),

            {ok, _EventId} =
                pl_storage_srv:save_event(achievement, AchievementId, PlayerId),

            multicast(<<"Achievement unlocked! 5 battles played!">>,
                      [PlayerPid]);

        _ ->
            ok
    end.

award_5_wins(PlayerId, PlayerPid) ->
    {ok, WinsAmount} = pl_storage_srv:get_won_battles(PlayerId),
    AchievementAlreadyReceived =
        pl_storage_srv:check_achievement_ownership(PlayerId, '5_wins'),

    case {WinsAmount, AchievementAlreadyReceived} of
        {_, true} ->
            ok;

        {WinsAmount, false} when WinsAmount >= 5 ->
            {ok, AchievementId} =
                pl_storage_srv:save_achievement(PlayerId, '5_wins'),

            {ok, _EventId} =
                pl_storage_srv:save_event(achievement, AchievementId, PlayerId),

            multicast(<<"Achievement unlocked! 5 battles won!">>,
                      [PlayerPid])
    end.

award_first_win(PlayerId, PlayerPid) ->
    {ok, WinsAmount} = pl_storage_srv:get_won_battles(PlayerId),
    AchievementAlreadyReceived =
        pl_storage_srv:check_achievement_ownership(PlayerId, first_win),

    case {WinsAmount, AchievementAlreadyReceived} of
        {_, true} ->
            ok;

        {WinsAmount, false} when WinsAmount >= 1 ->
            {ok, AchievementId} =
                pl_storage_srv:save_achievement(PlayerId, first_win),

            {ok, _EventId} =
                pl_storage_srv:save_event(achievement, AchievementId, PlayerId),

            multicast(<<"Achievement unlocked! First win!">>,
                      [PlayerPid])
    end.

check_round(#round{count = Count, status = Status}) ->
    case Count + 1 of
        Count2 when Count2 rem 4 == 0->
            Status2 = set_fire,
            lager:info("Round ~p with status ~p", [Count, Status]),
            #round{count= Count2, status = Status2};
        Count2 when (Count2+1) rem 4 == 0 ->
            Status2 = prepared_fire,
            lager:info("Round ~p with status ~p", [Count, Status]),
            #round{count= Count2, status = Status2};
        Count2 -> lager:info("Round ~p with status ~p", [Count, Status]),
            #round{count= Count2, status = Status}
    end.


set_fire(Msg, Field, #round{status = Status} = Round, Fire) ->
    Direction = [<<"North">>, <<"South">>, <<"West">>, <<"East">>],
    case Status of
        inactive ->
            {Msg, Field, Round, Fire};

        prepared_fire ->
            Wind = lists:nth(rand:uniform(length(Direction)), Direction),

            Msg1 = <<"The Wind is blowing from the ",
                     Wind/binary, "\n", Msg/binary>>,

            case maps:find(Wind, Fire) of
                {ok, Wind_Power} ->
                    {
                        Msg1,
                        Field,
                        Round#round{status = inactive},
                        maps:update(Wind, Wind_Power+1, Fire)
                    };

                error ->
                    {
                        Msg1,
                        Field,
                        Round#round{status = inactive},
                        maps:put(Wind, 0, Fire)
                    }
            end;

        set_fire ->
            Field2 = fire(Field, Fire),
            {<<Msg/binary>>, Field2, Round#round{status = inactive}, Fire}
    end.

fire(Field, Fire) ->
    {ok, Size} = application:get_env(polyana, battle_field_size),
    fire(maps:keys(Fire), Field, Fire, Size).

fire([], Field, _Fire, _Size) ->
    Field;

fire([<<"North">>|Directions],Field, Fire, Size) ->
    Y = maps:get(<<"North">>, Fire),
    Seq = lists:seq(0, Size),

    Field2 = lists:foldl(
        fun(X, Acc) ->
            case maps:find({Y,X}, Acc) of
                Value when Value == {ok, <<"O">>}; Value == {ok, <<"X">>} ->
                    Acc#{{Y,X}:= <<"F">>};

                _ ->
                    Acc
            end
        end,
        Field,
        Seq
    ),

fire(Directions, Field2, Fire, Size);

fire([<<"South">>|Directions],Field, Fire, Size) ->
    Y = maps:get(<<"South">>, Fire),
    Y2 = Size - Y,
    Seq = lists:seq(0, Size),

    Field2 = lists:foldl(
        fun(X, Acc) ->
            case maps:find({Y2,X}, Acc) of
                Value when Value == {ok, <<"O">>}; Value == {ok, <<"X">>} ->
                    Acc#{{Y2,X}:= <<"F">>};

                _ ->
                    Acc
            end
        end,
        Field,
        Seq
    ),

    fire(Directions, Field2, Fire, Size);

fire([<<"West">>|Directions],Field, Fire, Size) ->
    X = maps:get(<<"West">>, Fire),
    Seq = lists:seq(0, Size),

    Field2 = lists:foldl(
        fun(Y, Acc) ->
            case maps:find({Y,X}, Acc) of
                Value when Value == {ok, <<"O">>}; Value == {ok, <<"X">>} ->
                    Acc#{{Y,X}:= <<"F">>};

                _ ->
                    Acc
            end
        end,
        Field,
        Seq
    ),

    fire(Directions, Field2, Fire, Size);

fire([<<"East">>|Directions],Field, Fire, Size) ->
    X = maps:get(<<"East">>, Fire),
    X2 = Size - X,
    Seq = lists:seq(0, Size),
    Field2 = lists:foldl(
        fun(Y, Acc) ->
            case maps:find({Y,X2}, Acc) of
                Value when Value == {ok, <<"O">>}; Value == {ok, <<"X">>} ->
                    Acc#{{Y,X2}:= <<"F">>};

                _ ->
                    Acc
            end
        end,
        Field,
        Seq
    ),

    fire(Directions, Field2, Fire, Size).
