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
  M = 7,
  N = 7,
  First = {{0,0}, <<"A">>},
  Second = {{M,N}, <<"B">>},
  Positions = lists:foldl(fun (E, Acc)-> Acc#{E => none} end, #{}, Players),
  [B, A] = Players,
  Positions1 = Positions#{A:= First, B:=Second},
  State = #state{players = Positions1,
    size = {M, N},
    order = A},
  {ok, State}.

get_field(BattleSrv)->
  gen_server:call(BattleSrv, get_field).

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

handle_call(get_field, _From, #state{field = Field, size = Size}=State) ->
  {reply, {Field, Size}, State};

handle_call(new, _From, #state{size = {M, N}, players = Players} = State) ->
  Field = gen_field(M, N),
  Field2 = maps:fold(fun (_K, {Position, Mark},Acc)-> maps:update(Position, Mark, Acc) end, Field, Players),
  State2 = State#state{field = Field2},
  {reply, ok, State2};

handle_call({move, PlayerPid, Direction}, _From,
    #state{field = Field, size = Size, players = Players, order = Order} = State) ->
  {Flag, Msg, Field2, Players2, Order2} =
    move(PlayerPid, Direction, Field, Size, Players, Order),
  State2 = State#state{field = Field2, players = Players2, order = Order2},
  {reply, {Flag, Msg, Field2, Size}, State2};

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

move(PlayerPid, Direction, Field, Size, Players, Order) when Order == PlayerPid ->
  Player = maps:get(Order,Players),
  case Direction of
    <<"UP">> -> in_move(up, Field, Size, Player, Players, Order);
    <<"DOWN">> -> in_move(down, Field, Size, Player, Players, Order);
    <<"LEFT">> -> in_move(left, Field, Size, Player, Players, Order);
    <<"RIGHT">> -> in_move(right, Field, Size, Player, Players, Order);
    _ -> {ok, <<"WRONG COMMAND">>, Field, Players, Order}
  end;
move(PlayerPid, _Direction, Field, _Size, Players, Order) when Order =/= PlayerPid ->
  {ok, <<"Not your turn">>, Field, Players, Order}.

in_move(up, Field, _Size, {{Y, X}, Mark}, Players, Order) when Y == 0->
  {ok,<<"Border">>, Field, Players, Order};
in_move(up, Field, _Size, {{Y, X}, Mark}, Players, Order) ->
  case maps:find({Y-1, X}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y-1,X} => Mark},
      Players2 = maps:update(Order, {{Y-1,X}, Mark}, Players),
      Pids = maps:keys(Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order, Pids)};
    {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"ERROR">>, Field, Players, Order}
  end;

in_move(down, Field, {M, _N}, {{Y, X}, Mark}, Players, Order) when Y == M->
  {ok,<<"Border">>, Field, Players, Order};
in_move(down, Field, _Size, {{Y, X}, Mark}, Players, Order) ->
  case maps:find({Y+1, X}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y+1,X} => Mark},
      Players2 = maps:update(Order, {{Y+1,X}, Mark}, Players),
      Pids = maps:keys(Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order, Pids)};
     {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"ERROR">>, Field, Players, Order}
  end;

in_move(left, Field, _Size, {{Y, X}, Mark}, Players, Order) when X == 0->
  {ok, <<"Border">>, Field, Players, Order};
in_move(left, Field, _Size, {{Y, X}, Mark}, Players, Order) ->
  case maps:find({Y, X-1}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X-1} => Mark},
      Players2 = maps:update(Order, {{Y,X-1}, Mark}, Players),
      Pids = maps:keys(Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order, Pids)};
    {ok,_} -> {ok,<<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok,<<"ERROR">>, Field, Players, Order}
  end;

in_move(right, Field, {_M, N}, {{Y, X}, Mark}, Players, Order) when X == N->
  {ok,<<"Border">>, Field, Players, Order};
in_move(right, Field, _Size, {{Y, X}, Mark}, Players, Order) ->
  case maps:find({Y, X+1}, Field) of
    {ok,<<"O">>} -> Field2 = Field#{{Y,X} => <<"X">>, {Y,X+1} => Mark},
      Players2 = maps:update(Order, {{Y,X+1}, Mark}, Players),
      Pids = maps:keys(Players),
      {battle,<<"Next move">>, Field2, Players2, change_order(Order, Pids)};
    {ok,_} -> {ok, <<"WRONG SPACE">>, Field, Players, Order};
    error -> {ok, <<"ERROR">>, Field, Players, Order}
  end.


gen_field(M, N)-> gen_field(M, N, #{}, M, N).
gen_field(_M, -1, Acc, _M_orig, _N_orig) -> Acc;
gen_field(-1, N, Acc, M_orig, N_orig) ->
  gen_field(M_orig, N-1, Acc, M_orig, N_orig);
gen_field(M, N, Acc, M_orig, N_orig) ->
  Acc2 = Acc#{{M,N}=> <<"O">>},
  gen_field(M-1, N, Acc2, M_orig, N_orig).

change_order(Order, Pids)->
  [Order2] = lists:filter(fun(E) -> if E =/= Order -> true; E==Order-> false end end, Pids),
  Order2.

