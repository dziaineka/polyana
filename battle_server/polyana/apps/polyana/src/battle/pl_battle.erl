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
-export([gen_field/2, change_order/1, set_map/3]).
-export([send_messages/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    players_positions,
    battle_field,
    battle_field_size,
    turn_order
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
    {Players, _Currencies} = lists:unzip(PlayersWithCurrencies),
    FieldHeight = 2,
    FieldWidth = 2,

    Values = [{{0, 0}, <<"A">>},
              {{FieldHeight, FieldWidth}, <<"B">>},
              {{FieldHeight, 0}, <<"C">>},
              {{0, FieldWidth}, <<"D">>}],

    lists:foreach(
        fun(PlayerSrv) ->
            pl_player_srv:add_battle_pid(PlayerSrv, self())
        end,
        Players
    ),

    Order = shuffle(Players),
    PlayersPos = set_map(Players, Values, #{}),
    Field = gen_field(FieldHeight, FieldWidth),

    Field2 = maps:fold(
        fun (_K, {Position, Mark},Acc) ->
            maps:update(Position, Mark, Acc)
        end,
        Field,
        PlayersPos
    ),

    Field3 = field_to_msg(Field2, {FieldHeight, FieldWidth}),
    send_messages(PlayersPos, Order, Field3),

    save_start_game_data(CurrencyType, Bid, PlayersWithCurrencies),

    State = #state{players_positions = PlayersPos,
                   battle_field = Field2,
                   battle_field_size = {FieldHeight, FieldWidth},
                   turn_order = Order},

    {ok, State}.

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
        {Active, PlayersDict} ->
            Active ! {message, <<Reply/binary, "It's your turn", "\n">>},

            PlayersList = maps:keys(PlayersDict),
            multicast(Reply, PlayersList);

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

send_messages(Players, [First|_Order], Raw_Field) ->
    Field = list_to_binary(Raw_Field),
    PlayersPid = maps:keys(Players),

    lists:foreach(
        fun
            (PlayerPid) when PlayerPid == First ->
                {_, Mark} = maps:get(First, Players),
                PlayerPid ! {message,
                            <<Field/binary, "Now turn player ",
                            Mark/binary, "\n", "It's your turn", "\n">>};
            (PlayerPid) ->
                {_, Mark} = maps:get(First, Players),

                PlayerPid ! {message, <<Field/binary, "Now turn player ",
                                        Mark/binary, "\n">>}
        end,
        PlayersPid
    ).


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
                   players_positions = PlayersPos,
                   turn_order = Order} = State) ->
    {Flag, Msg, Field2, NewPlayersPos, Order2} =
        move(PlayerPid, Direction, Field, PlayersPos, Order),

    PlayersList = maps:keys(PlayersPos),

    case Flag of
        nok ->
            {reply, {Flag, Msg, field_to_msg(Field, Size), NewPlayersPos}, State};

        single ->
            [First| _Others]=Order,
            {reply, {Flag, Msg, field_to_msg(Field, Size), First}, State};

        multi ->
            Order3 = lose_condition(Field2, NewPlayersPos, Order2, Order2),
            NewBattleField = field_to_msg(Field2, Size),

            case win_condition(Order3, NewPlayersPos) of
                next ->
                    State2 = State#state{battle_field = Field2,
                                         players_positions = NewPlayersPos,
                                         turn_order = Order3},

                    [Next_player|_] = Order3,
                    {_, Mark} = maps:get(Next_player, NewPlayersPos),
                    Msg2 = <<Msg/binary, " player ", Mark/binary>>,

                    {
                        reply,
                        {Flag, Msg2, NewBattleField, {Next_player,
                                                      NewPlayersPos}},
                        State2
                    };

                WinMessage ->
                    State2 = State#state{battle_field = Field2,
                                         players_positions = NewPlayersPos,
                                         turn_order = Order3},

                    {
                        reply,
                        {win, WinMessage, NewBattleField, PlayersList},
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

move(PlayerPid, Direction, Field, PlayersPos,
            [Active|_Passive] = Order) when Active == PlayerPid ->
    Player = maps:get(Active,PlayersPos),
    in_move(Direction, Field, Player, PlayersPos, Order);

move(PlayerPid, _Direction, Field, _PlayersPos,
            [Active|_Passive]=Order) when Active=/= PlayerPid ->
    {nok, <<"Not your turn">>, Field, PlayerPid, Order}.


in_move(up, Field, {{Y, X}, Mark}, PlayersPos, [Active|_Passive] = Order) ->
    case maps:find({Y-1, X}, Field) of
        {ok,<<"O">>} ->
            Field2 = Field#{{Y, X} => <<"X">>, {Y-1,X} => Mark},
            PlayersPos2 = maps:update(Active, {{Y-1,X}, Mark}, PlayersPos),
            {multi,<<"Next move">>, Field2, PlayersPos2, change_order(Order)};

        {ok,_} ->
            {single,<<"WRONG SPACE">>, Field, PlayersPos, Order};

        error ->
            {single,<<"Border">>, Field, PlayersPos, Order}
    end;

in_move(down, Field, {{Y, X}, Mark}, PlayersPos, [Active|_Passive]=Order) ->
    case maps:find({Y+1, X}, Field) of
        {ok,<<"O">>} ->
            Field2 = Field#{{Y, X} => <<"X">>, {Y+1,X} => Mark},
            PlayersPos2 = maps:update(Active, {{Y+1,X}, Mark}, PlayersPos),
            {multi,<<"Next move">>, Field2, PlayersPos2, change_order(Order)};

        {ok,_} ->
            {single,<<"WRONG SPACE">>, Field, PlayersPos, Order};

        error ->
            {single,<<"Border">>, Field, PlayersPos, Order}
    end;

in_move(left, Field, {{Y, X}, Mark}, PlayersPos, [Active|_Passive]=Order) ->
    case maps:find({Y, X-1}, Field) of
        {ok,<<"O">>} ->
            Field2 = Field#{{Y, X} => <<"X">>, {Y,X-1} => Mark},
            PlayersPos2 = maps:update(Active, {{Y,X-1}, Mark}, PlayersPos),
            {multi,<<"Next move">>, Field2, PlayersPos2, change_order(Order)};

        {ok,_} ->
            {single,<<"WRONG SPACE">>, Field, PlayersPos, Order};

        error ->
            {single, <<"Border">>, Field, PlayersPos, Order}
    end;

in_move(right, Field, {{Y, X}, Mark}, PlayersPos, [Active|_Passive]=Order) ->
    case maps:find({Y, X+1}, Field) of
        {ok,<<"O">>} ->
            Field2 = Field#{{Y, X} => <<"X">>, {Y,X+1} => Mark},
            PlayersPos2 = maps:update(Active, {{Y,X+1}, Mark}, PlayersPos),
            {multi,<<"Next move">>, Field2, PlayersPos2, change_order(Order)};

        {ok,_} ->
            {single, <<"WRONG SPACE">>, Field, PlayersPos, Order};

        error ->
            {single,<<"Border">>, Field, PlayersPos, Order}
    end.


gen_field(FieldHeight, FieldWidth) ->
    gen_field(FieldHeight, FieldWidth, #{}, FieldHeight, FieldWidth).


gen_field(_M, -1, Acc, _M_orig, _N_orig) ->
    Acc;

gen_field(-1, FieldWidth, Acc, FieldHeight_orig, FieldWidth_orig) ->
    gen_field(FieldHeight_orig, FieldWidth-1,
              Acc,
              FieldHeight_orig, FieldWidth_orig);

gen_field(FieldHeight, FieldWidth, Acc, FieldHeight_orig, FieldWidth_orig) ->
    Acc2 = Acc#{{FieldHeight,FieldWidth}=> <<"O">>},

    gen_field(FieldHeight-1, FieldWidth,
              Acc2,
              FieldHeight_orig, FieldWidth_orig).


change_order([Active|Passive])->
    Order = lists:append(Passive, [Active]),
    Order.


lose_condition(_Field, _Players, _, Order) when length(Order) == 1 ->
    Order;

lose_condition(_Field, _Players, [], Order) ->
    Order;

lose_condition(Field, Players, [Active|Others], Order)->
    {Position, Mark}= maps:get(Active, Players),
    Directions = [up, down, left, right],

    case find_direction(Field, Position, Directions) of
        ok ->
            lose_condition(Field, Players, Others, Order);

        lose ->
            lager:info("Player ~p lose", [Mark]),
            %добавить команду для изменения ретинга проигравшего игрока
            Order2 = lists:delete(Active, Order),
            lose_condition(Field, Players, Others, Order2)
    end.


win_condition(Order, Players) when length(Order) == 1 ->
    [Player] = Order,
    {_Position, Mark} = maps:get(Player, Players),
  %%добавить команду для изменения ретинга выигревшего игрока
    <<Mark/binary, " wins. Congratulations!">>;

win_condition(_Order, _Players) ->
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


set_map([], _Values, MapAcc) ->
    MapAcc;

set_map([Player | Players], [Value | Values], MapAcc) ->
    set_map(Players, Values, MapAcc#{Player => Value}).
