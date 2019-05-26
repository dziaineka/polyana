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
    players_info,
    battle_field,
    battle_field_size,
    turn_order,
    bid,
    currency_type
}).

-record(player_info, {
    position,
    mark,
    currency_type
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
    Order = shuffle(Players),
    PlayersInfo = get_players_info(Players, Currencies),
    Field = create_field(PlayersInfo),
    StringField = field_to_msg(Field, {FieldHeight, FieldWidth}),
    save_start_game_data(CurrencyType, Bid, PlayersWithCurrencies),
    invite_players_to_game(PlayersInfo, Order, StringField),

    State = #state{players_info = PlayersInfo,
                   battle_field = Field,
                   battle_field_size = {FieldHeight, FieldWidth},
                   turn_order = Order,
                   bid = Bid,
                   currency_type = CurrencyType},

    {ok, State}.

send_battle_pid(Players) ->
    lists:foreach(
        fun(PlayerSrv) ->
            pl_player_srv:add_battle_pid(PlayerSrv, self())
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
    {Flag, Msg, Raw_Field, Pids} =
        gen_server:call(BattlePid, {move, PlayerPid, Direction}),

    Reply = <<(list_to_binary(Raw_Field))/binary, Msg/binary, "\n">>,

    case Flag of
        win ->
            multicast(Reply, Pids),
            lager:info("stop room ~p", [self()]),
            stop(BattlePid);

        multi ->
            multicast(Reply, Pids);

        _ ->
            multicast(Reply, [Pids])
    end.


stop(BattleSrv)->
    lager:info("stop room ~p", [BattleSrv]),
    gen_server:call(BattleSrv, stop).

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
    ).

invite_players_to_game(PlayersInfo, [First|_Order], Raw_Field) ->
    Players = maps:keys(PlayersInfo),
    Field = list_to_binary(Raw_Field),

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

handle_call({move, PlayerPid, Direction},
            _From,
            #state{battle_field = Field,
                   battle_field_size = Size,
                   players_info = PlayersInfo,
                   turn_order = Order} = State) ->
    {Flag, Msg, Field2, NewPlayersInfo, Order2} =
        move(PlayerPid, Direction, Field, PlayersInfo, Order),

    case Flag of
        nok ->
            {reply, {Flag, Msg, field_to_msg(Field, Size), NewPlayersInfo}, State};

        single ->
            [First| _Others] = Order,
            {reply, {Flag, Msg, field_to_msg(Field, Size), First}, State};

        multi ->
            Order3 = lose_condition(Field2, NewPlayersInfo, Order2, Order2),
            NewField = field_to_msg(Field2, Size),
            NewPlayersList = maps:keys(NewPlayersInfo),

            case win_condition(Order3, NewPlayersInfo) of
                next ->
                    State2 = State#state{battle_field = Field2,
                                         players_info = NewPlayersInfo,
                                         turn_order = Order3},

                    [NextPlayer | _] = Order3,

                    #player_info{mark = Mark} =
                        maps:get(NextPlayer, NewPlayersInfo),

                    Msg2 = <<Msg/binary, " player ", Mark/binary>>,

                    {
                        reply,
                        {
                            Flag,
                            Msg2,
                            NewField,
                            {NextPlayer, NewPlayersList}
                        },
                        State2
                    };

                {_WinnerPid, WinMessage} ->
                    % save_end_game_data(WinnerPid,
                    %                    maps:keys(NewPlayersInfo),
                    %                    ),

                    State2 = State#state{battle_field = Field2,
                                         players_info = NewPlayersInfo,
                                         turn_order = Order3},

                    {
                        reply,
                        {win, WinMessage, NewField, NewPlayersList},
                        State2
                    }
            end
    end;


handle_call(stop, _From, State) ->
    lager:info("stop room ~p", [self()]),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

            NewPlayerInfo = maps:update(
                Active,
                PlayerInfo#player_info{position = NewPosition},
                PlayersInfo),

            {
                multi,
                <<"Next turn">>,
                Field2,
                NewPlayerInfo,
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
            %добавить команду для изменения ретинга проигравшего игрока
            Order2 = lists:delete(Active, Order),
            lose_condition(Field, PlayersInfo, Others, Order2)
    end.


win_condition(Order, PlayersInfo) when length(Order) == 1 ->
    [PlayerPid] = Order,
    #player_info{mark = Mark} = maps:get(PlayerPid, PlayersInfo),
  %%добавить команду для изменения ретинга выигревшего игрока
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


field_to_msg(Raw_Field, {M, _})->
    Field= new_field(Raw_Field, M),
    Field1 = maps:to_list(Field),
    Field2 = lists:keysort(1, Field1),
    lists:map(fun ({_K, V})-> V end, Field2).


new_field(A, -1)->
    A;

new_field(A, M)->
    new_field(A#{{M, a} => <<"\n">>}, M - 1).


shuffle(List)->
    Random_list = [{rand:uniform(), X} || X <- List],
    [X || {_, X}<- lists:sort(Random_list)].


get_players_info(Players, Currencies) ->
    get_players_info(Players, Currencies, #{}).

get_players_info([], _Currencies, PlayersInfo) ->
    PlayersInfo;

get_players_info([Player | Players],
                 [Currency | Currencies],
                 PlayersInfo) ->
    PlayerInfo = #player_info{
                        currency_type = Currency,
                        position = get_initial_field_pos(length(Players) + 1),
                        mark = get_mark(length(Players) + 1)},

    get_players_info(Players, Currencies, PlayersInfo#{Player => PlayerInfo}).

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