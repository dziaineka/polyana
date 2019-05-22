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
-export([stop/1, get_field/1, command/2, move/3, get_players/1]).
-export([gen_field/2, change_order/1]).
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
  M = 1,
  N = 1,
  First = {{0,0}, <<"A">>},
  Second = {{M,N}, <<"B">>},
  Positions = lists:foldl(fun (E, Acc)-> Acc#{E => none} end, #{}, Players),
  [B, A] = Players,
  Positions1 = Positions#{A:= First, B:=Second},
  State = #state{players = Positions1,
    size = {M, N},
    order = Players},
  {ok, State}.

command(new, BattleSrv) ->
  gen_server:call(BattleSrv, new).

move(BattleSrv, PlayerPid, Direction)->
  gen_server:call(BattleSrv, {move, PlayerPid, Direction}).

stop(BattleSrv)->
  gen_server:call(BattleSrv, stop).

get_players(BattleSrv) ->
  gen_server:call(BattleSrv, get_players).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(new, _From, #state{size = {M, N}, players = Players} = State) ->
  Field = gen_field(M, N),
  Field2 = maps:fold(fun (_K, {Position, Mark},Acc)-> maps:update(Position, Mark, Acc) end, Field, Players),
  State2 = State#state{field = Field2},
  {reply, ok, State2};

handle_call({move, PlayerPid, Direction}, _From,
    #state{field = Field, size = Size, players = Players, order = Order} = State) ->
  {Flag, Msg, Field2, Players2, Order2} =
    move(PlayerPid, Direction, Field, Players, Order),
  case Flag of
    ok ->
      {reply, {Flag, Msg, good_field(Field, Size)}, State};
    battle -> Order3 = lose_condition(Field2, Players2, Order2, Order2),
      case win_condition(Order3, Players2) of
        next -> State2 = State#state{field = Field2, players = Players2, order = Order2},
          New_Field = good_field(Field2, Size),
          {reply, {Flag, Msg, New_Field}, State2};
        Win -> State2 = State#state{field = Field2, players = Players2, order = Order3},
          [Player] = Order3,
          Player ! close_room,
          New_Field = good_field(Field2, Size),
          {reply, {win, Win, New_Field}, State2}
      end
%%      State2 = State#state{field = Field2, players = Players2, order = Order2},
%%      New_Field = good_field(Field2, Size),
%%      {reply, {Flag, Msg, New_Field}, State2}
  end;

handle_call(get_players, _From,
    #state{players = Position} = State) ->
  Players = maps:keys(Position),
  {reply, Players, State};


handle_call(stop, _From, State) ->
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
move(PlayerPid, _Direction, Field, Players, [Active|_Passive]=Order) when Active=/= PlayerPid ->
  {ok, <<"Not your turn">>, Field, Players, Order}.

in_move(up, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
  case maps:find({Y-1, X}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y-1,X} => Mark},
      Players2 = maps:update(Active, {{Y-1,X}, Mark}, Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order)};
    {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"Border">>, Field, Players, Order}
  end;

in_move(down, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
  case maps:find({Y+1, X}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y+1,X} => Mark},
      Players2 = maps:update(Active, {{Y+1,X}, Mark}, Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order)};
    {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"Border">>, Field, Players, Order}
  end;

in_move(left, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
  case maps:find({Y, X-1}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X-1} => Mark},
      Players2 = maps:update(Active, {{Y,X-1}, Mark}, Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order)};
    {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok, <<"Border">>, Field, Players, Order}
  end;

in_move(right, Field, {{Y, X}, Mark}, Players, [Active|_Passive]=Order) ->
  case maps:find({Y, X+1}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X+1} => Mark},
      Players2 = maps:update(Active, {{Y,X+1}, Mark}, Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order)};
    {ok,_} -> {ok, <<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"Border">>, Field, Players, Order}
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


lose_condition(_Field, _Players, [], _Order) -> ok;
lose_condition(Field, Players, [Active|Others], Order)->
  {Position, Mark}= maps:get(Active, Players),
  Directions = [up, down, left, right],
  case find_direction(Field, Position, Directions) of
    ok -> lose_condition(Field, Players, Others, Order);
    lose -> lager:info("Player ~p lose", [Mark]),
      %добавить команду для изменения ретинга проигравшего игрока
      lists:delete(Active, Order)
  end.


win_condition(Order, Players) when length(Order) == 1->
  [Player] = Order,
  {_Position, Mark} = maps:get(Player, Players),
  lager:info("WINNER"),
  %%добавить команду для изменения ретинга выигревшего игрока
  <<Mark/binary, " wins. Congratulations">>;
win_condition(Order, _Players) ->
  lager:info("NEXT MOVE, ~p", [Order]),
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


good_field(Raw_Field, {M, _})->
  Field= new_field(Raw_Field, M),
  Field1 = maps:to_list(Field),
  Field2 = lists:keysort(1, Field1),
  lists:map(fun ({_K, V})-> V end, Field2).


new_field(A, -1)-> A;
new_field(A, M)-> new_field(A#{{M,a} => <<"\n">>}, M-1).

