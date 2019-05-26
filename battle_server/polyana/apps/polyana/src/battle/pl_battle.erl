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
-export([stop/1, move/3, multicast/2]).
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
    players,
    field,
    size,
    order
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

init(Players) ->
%%  {ok,{M,N}} = application:get_env(polyana, field_size),
    M = 2,
    N = 2,
    Values = [{{0,0}, <<"A">>}, {{M,N}, <<"B">>}, {{M,0}, <<"C">>},{{0, N}, <<"D">>}],
    lists:foreach(fun(PlayerSrv) ->
        pl_player_srv:add_battle_pid(PlayerSrv, self()) end, Players),
    Order = shuffle(Players),
    Players2 = set_map(Players, Values, #{}),
    Field = gen_field(M, N),
    Field2 = maps:fold(fun (_K, {Position, Mark},Acc)-> maps:update(Position, Mark, Acc) end, Field, Players2),
    Field3 = field_to_msg(Field2, {M,N}),
    send_messages(Players2, Order, Field3),
    State = #state{players = Players2,
        size = {M, N},
        order = Order,
        field = Field2},
    {ok, State}.

move(BattlePid, PlayerPid, Direction)->
    {Flag, Msg, Raw_Field, Pids} = gen_server:call(BattlePid, {move, PlayerPid, Direction}),
    Field = list_to_binary(Raw_Field),
    multicast({Flag, <<Field/binary, Msg/binary, "\n">>, Pids}, BattlePid).

stop(BattleSrv)->
    lager:info("stop room ~p", [BattleSrv]),
    gen_server:call(BattleSrv, stop).

multicast(Raw_Reply, BattlePid) ->
    case Raw_Reply of
        {win, Reply, Players} ->
            lager:info("win message ~p", [Reply]),
            lists:foreach(fun(PlayerSrv) ->
                PlayerSrv ! exit_room,
                PlayerSrv ! {message, Reply} end, Players),
                lager:info("stop room ~p", [self()]),
                stop(BattlePid);
        {multi, Reply, {Active, PlayersDict}} ->
            Players = maps:keys(PlayersDict),
            lager:info("multi  message ~p", [Reply]),
            lists:foreach(fun(PlayerSrv) when PlayerSrv == Active ->
                PlayerSrv ! {message, <<Reply/binary, "It's your turn", "\n">>};
                (PlayerSrv) -> PlayerSrv ! {message, Reply} end, Players);
        {single, Reply, First} ->
            lager:info("single message ~p", [Reply]),
            First ! {message, Reply};
        {nok, Reply, Pid}->
            lager:info("error message ~p", [Reply]),
            Pid ! {message, Reply}
    end.

send_messages(Players, [First|_Order], Raw_Field)->
    Field = list_to_binary(Raw_Field),
    PlayersPid = maps:keys(Players),
    lists:foreach(fun (PlayerPid) when PlayerPid == First->
        {_, Mark} = maps:get(First, Players),
            PlayerPid ! {message,
            <<Field/binary, "Now turn player ", Mark/binary, "\n", "It's your turn", "\n">>};
        (PlayerPid) ->
            {_, Mark} = maps:get(First, Players),
            PlayerPid ! {message, <<Field/binary, "Now turn player ", Mark/binary, "\n">>}
            end, PlayersPid).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({move, PlayerPid, Direction}, _From,
    #state{field = Field, size = Size, players = Players, order = Order} = State) ->
    {Flag, Msg, Field2, Players2, Order2} =
        move(PlayerPid, Direction, Field, Players, Order),
    PlayersList = maps:keys(Players),
    case Flag of
        nok -> {reply, {Flag, Msg, field_to_msg(Field, Size), Players2}, State};
        single -> [First| _Others]=Order,
            {reply, {Flag, Msg, field_to_msg(Field, Size), First}, State};
        multi -> Order3 = lose_condition(Field2, Players2, Order2, Order2),
            case win_condition(Order3, Players2) of
                next -> State2 = State#state{field = Field2, players = Players2, order = Order3},
                    New_Field = field_to_msg(Field2, Size),
                    [Next_player|_] = Order3,
                    {_, Mark} = maps:get(Next_player, Players2),
                    Msg2 = <<Msg/binary, " player ", Mark/binary>>,
                    {reply, {Flag, Msg2, New_Field, {Next_player, Players2}}, State2};
                Win -> State2 = State#state{field = Field2, players = Players2, order = Order3},
                    New_Field = field_to_msg(Field2, Size),
                    {reply, {win, Win, New_Field, PlayersList}, State2}
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

move(PlayerPid, Direction, Field, Players, [Active|_Passive]=Order) when Active== PlayerPid ->
    Player = maps:get(Active,Players),
    in_move(Direction, Field, Player, Players, Order);
move(PlayerPid, _Direction, Field, _Players, [Active|_Passive]=Order) when Active=/= PlayerPid ->
    {nok, <<"Not your turn">>, Field, PlayerPid, Order}.

in_move(up, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
    case maps:find({Y-1, X}, Field) of
        {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y-1,X} => Mark},
            Players2 = maps:update(Active, {{Y-1,X}, Mark}, Players),
            {multi,<<"Next move">>, Field2, Players2, change_order(Order)};
        {ok,_} -> {single,<<"WRONG SPACE">>, Field, Players, Order};
        error -> {single,<<"Border">>, Field, Players, Order}
    end;

in_move(down, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
    case maps:find({Y+1, X}, Field) of
        {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y+1,X} => Mark},
            Players2 = maps:update(Active, {{Y+1,X}, Mark}, Players),
            {multi,<<"Next move">>, Field2, Players2, change_order(Order)};
        {ok,_} -> {single,<<"WRONG SPACE">>, Field, Players, Order};
        error -> {single,<<"Border">>, Field, Players, Order}
    end;

in_move(left, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
    case maps:find({Y, X-1}, Field) of
        {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X-1} => Mark},
            Players2 = maps:update(Active, {{Y,X-1}, Mark}, Players),
            {multi,<<"Next move">>, Field2, Players2, change_order(Order)};
        {ok,_} -> {single,<<"WRONG SPACE">>, Field, Players, Order};
        error -> {single, <<"Border">>, Field, Players, Order}
    end;

in_move(right, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
    case maps:find({Y, X+1}, Field) of
        {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X+1} => Mark},
            Players2 = maps:update(Active, {{Y,X+1}, Mark}, Players),
            {multi,<<"Next move">>, Field2, Players2, change_order(Order)};
        {ok,_} -> {single, <<"WRONG SPACE">>, Field, Players, Order};
        error -> {single,<<"Border">>, Field, Players, Order}
    end.


gen_field(M, N)-> gen_field(M, N, #{}, M, N).
gen_field(_M, -1, Acc, _M_orig, _N_orig) -> Acc;
gen_field(-1, N, Acc, M_orig, N_orig) ->
    gen_field(M_orig, N-1, Acc, M_orig, N_orig);
gen_field(M, N, Acc, M_orig, N_orig) ->
    Acc2 = Acc#{{M,N}=> <<"O">>},
    gen_field(M-1, N, Acc2, M_orig, N_orig).

change_order([Active|Passive])->
    Order = lists:append(Passive, [Active]),
    Order.

lose_condition(_Field, _Players, _, Order) when length(Order) == 1-> Order;
lose_condition(_Field, _Players, [], Order) -> Order;
lose_condition(Field, Players, [Active|Others], Order)->
    {Position, Mark}= maps:get(Active, Players),
    Directions = [up, down, left, right],
    case find_direction(Field, Position, Directions) of
        ok -> lose_condition(Field, Players, Others, Order);
        lose -> lager:info("Player ~p lose", [Mark]),
            %добавить команду для изменения ретинга проигравшего игрока
            Order2 = lists:delete(Active, Order),
            lose_condition(Field, Players, Others, Order2)
    end.

win_condition(Order, Players) when length(Order) == 1->
    [Player] = Order,
    {_Position, Mark} = maps:get(Player, Players),
  %%добавить команду для изменения ретинга выигревшего игрока
    <<Mark/binary, " wins. Congratulations">>;
win_condition(_Order, _Players) ->
    next.


find_direction(_Field, {_Y,_X}, [])->
    lose;
find_direction(Field, {Y,X}, [up|Directions]) ->
    case maps:find({Y-1, X}, Field) of
        {ok,<<"O">>} -> ok;
        {ok,_} -> find_direction(Field, {Y,X}, Directions);
        error -> find_direction(Field, {Y,X}, Directions)
    end;
find_direction(Field, {Y,X},[down|Directions]) ->
    case maps:find({Y+1, X}, Field) of
        {ok,<<"O">>} -> ok;
        {ok,_} -> find_direction(Field, {Y,X}, Directions);
        error -> find_direction(Field, {Y,X}, Directions)
    end;
find_direction(Field, {Y,X}, [left|Directions])->
    case maps:find({Y, X-1}, Field) of
        {ok,<<"O">>} -> ok;
        {ok,_} -> find_direction(Field, {Y,X}, Directions);
        error -> find_direction(Field, {Y,X}, Directions)
    end;
find_direction(Field, {Y,X}, [right|Directions]) ->
    case maps:find({Y, X+1}, Field) of
        {ok,<<"O">>} -> ok;
        {ok,_} -> find_direction(Field, {Y,X}, Directions);
        error -> find_direction(Field, {Y,X}, Directions)
    end.

field_to_msg(Raw_Field, {M, _})->
    Field= new_field(Raw_Field, M),
    Field1 = maps:to_list(Field),
    Field2 = lists:keysort(1, Field1),
    lists:map(fun ({_K, V})-> V end, Field2).

new_field(A, -1)-> A;
new_field(A, M)-> new_field(A#{{M,a} => <<"\n">>}, M-1).

shuffle(List)->
    Random_list = [{rand:uniform(), X}|| X <- List],
    [X||{_,X}<- lists:sort(Random_list)].

set_map([], _Values, MapAcc) -> MapAcc;
set_map([Player|Players], [Value|Values], MapAcc) ->
    set_map(Players, Values, MapAcc#{Player => Value}).
