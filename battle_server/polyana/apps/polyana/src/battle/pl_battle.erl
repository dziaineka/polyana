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
    winner = undefined
}).

-record(player_info, {
    position,
    mark,
    currency_type,
    id
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

    State = #state{battle_id = BattleId,
                   players_info = PlayersInfo,
                   battle_field = Field,
                   battle_field_size = {FieldHeight, FieldWidth},
                   turn_order = Order,
                   bid = Bid,
                   currency_type = CurrencyType},

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
                turn_order = Order} = State) ->
    {Flag, Msg, NewField, NewPlayersInfo, NewOrder} =
        move(PlayerPid, Direction, Field, PlayersInfo, Order),

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
                                    Size, State, Msg)
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
                   battle_field_size = Size} = State) ->
    lager:info("Player down, he is lost ~p ~p", [PlayerPid, Info]),
    #player_info{mark = Mark} = maps:get(PlayerPid, PlayersInfo),

    % multicast([<<"Player ",Mark/binary, " is gone.\n", "Send any command.\n">>],
    %           TurnOrder),

    Msg = <<"Player ",Mark/binary, " is gone.\n">>,

    check_battle_conditions(BattleField, PlayersInfo, TurnOrder,
                            Size, State, Msg),

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

check_battle_conditions(NewField, NewPlayersInfo, NewOrder, Size, State, Msg) ->
    Order3 = lose_condition(NewField, NewPlayersInfo, NewOrder, NewOrder),
    StringField = field_to_msg(NewField, Size),
    NewPlayersList = maps:keys(NewPlayersInfo),

    case win_condition(Order3, NewPlayersInfo) of
        next ->
            State2 = State#state{battle_field = NewField,
                                players_info = NewPlayersInfo,
                                turn_order = Order3},

            [NextPlayer | _] = Order3,

            #player_info{mark = Mark} =
                maps:get(NextPlayer, NewPlayersInfo),

            Msg2 = <<Msg/binary, " player ", Mark/binary>>,
            Reply = <<(list_to_binary(StringField))/binary, Msg2/binary, "\n">>,
            multicast(Reply, {NextPlayer, NewPlayersList}),
            {noreply, State2};

        {WinnerPid, WinMessage} ->
            State2 = State#state{battle_field = NewField,
                                players_info = NewPlayersInfo,
                                turn_order = Order3,
                                winner = WinnerPid},

            Reply = <<(list_to_binary(StringField))/binary, WinMessage/binary, "\n">>,
            multicast(Reply, NewPlayersList),

            stop(self()),

            {noreply, State2}
    end.

move(PlayerPid, Direction, Field, PlayersInfo,
            [Active|_Passive] = Order) when Active == PlayerPid ->
    PlayerInfo = maps:get(Active, PlayersInfo),
    {Y, X} = PlayerInfo#player_info.position,

    case Direction of
        up ->
            in_move(Field, {Y - 1, X}, PlayersInfo, Order);

        down ->
            in_move(Field, {Y + 1, X}, PlayersInfo, Order);

        left ->
            in_move(Field, {Y, X - 1}, PlayersInfo, Order);

        right ->
            in_move(Field, {Y, X + 1}, PlayersInfo, Order)
    end;

move(PlayerPid, _Direction, Field, _PlayersPos,
            [Active|_Passive]=Order) when Active=/= PlayerPid ->
    {nok, <<"Not your turn">>, Field, PlayerPid, Order}.


in_move(Field, NewPosition, PlayersInfo, [Active | Passive]) ->
    PlayerInfo = maps:get(Active, PlayersInfo),

    case maps:find(NewPosition, Field) of
        {ok, <<"O">>} ->
            Field2 = Field#{PlayerInfo#player_info.position => <<"X">>,
                            NewPosition => PlayerInfo#player_info.mark},

            NewPlayersInfo = maps:update(
                Active,
                PlayerInfo#player_info{position = NewPosition},
                PlayersInfo),

            {
                multi,
                <<"Next turn">>,
                Field2,
                NewPlayersInfo,
                change_order([Active | Passive])
            };

        _ ->
            {single, <<"NO WAY">>, Field, [Active], [Active | Passive]}
    end.


gen_empty_field(FieldHeight, FieldWidth) ->
    gen_empty_field(FieldHeight, FieldWidth, #{}, FieldHeight, FieldWidth).


gen_empty_field(_M, -1, Acc, _M_orig, _N_orig) ->
    Acc;

gen_empty_field(-1, FieldWidth, Acc, FieldHeight_orig, FieldWidth_orig) ->
    gen_empty_field(FieldHeight_orig, FieldWidth-1,
              Acc,
              FieldHeight_orig, FieldWidth_orig);

gen_empty_field(FieldHeight, FieldWidth, Acc, FieldHeight_orig, FieldWidth_orig) ->
    Acc2 = Acc#{{FieldHeight,FieldWidth}=> <<"O">>},

    gen_empty_field(FieldHeight-1, FieldWidth,
              Acc2,
              FieldHeight_orig, FieldWidth_orig).


change_order([Active|Passive])->
    Order = lists:append(Passive, [Active]),
    Order.


lose_condition(_Field, _PlayersInfo, _, Order) when length(Order) == 1 ->
    Order;

lose_condition(_Field, _PlayersInfo, [], Order) ->
    Order;

lose_condition(Field, PlayersInfo, [Active|Others], Order)->
    #player_info{position = Position, mark = Mark} =
        maps:get(Active, PlayersInfo),

    Directions = [up, down, left, right],

    case find_direction(Field, Position, Directions) of
        ok ->
            lose_condition(Field, PlayersInfo, Others, Order);

        lose ->
            lager:info("Player ~p lose", [Mark]),
            Order2 = lists:delete(Active, Order),
            multicast(<<"You lose!\n">>, [Active]),
            lose_condition(Field, PlayersInfo, Others, Order2)
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

    get_players_info(Players, Currencies, PlayersInfo#{PlayerPid => PlayerInfo}).

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
    WinnerId = pl_player_srv:get_id(WinnerPid),
    ok = pl_storage_srv:save_winner(BattleId, WinnerId),

    lists:foreach(
        fun ({_PlayerPid, #player_info{currency_type = PlayerCurrencyType,
                                       id = PlayerId}}) ->
            % сохраним событие о конце игры
            {ok, EventId} = pl_storage_srv:save_event(battle_end,
                                                      BattleId,
                                                      PlayerId),

            case (WinnerId == PlayerId) of
                true ->
                    % добавим единичку к выигранным
                    pl_storage_srv:add_won_game(PlayerId),
                    % пересчитаем винрейт
                    pl_storage_srv:update_winrate(PlayerId),

                    % переведем победителю банк
                    pl_storage_srv:save_transaction(
                        EventId,
                        WinnerId,
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