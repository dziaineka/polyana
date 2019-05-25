-module(pl_player_srv).
-behavior(gen_server).

-export([start_link/1, auth/3, auth/2, start_battle/2, get_id/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(ReplyFun) ->
    gen_server:start_link(?MODULE, ReplyFun, []).

auth(PlayerSrv, Login, Pass) ->
    gen_server:call(PlayerSrv, {auth, Login, Pass}).

auth(PlayerSrv, Token) ->
    gen_server:call(PlayerSrv, {auth, Token}).

start_battle(PlayerSrv, {Currency, Bid}) ->
    gen_server:call(PlayerSrv, {start_battle, {Currency, Bid}}).

get_id(PlayerSrv) ->
    gen_server:call(PlayerSrv, get_id).


stop(PlayerSrv) ->
    gen_server:call(PlayerSrv, stop).


%%% gen_server API

-record(state, {
    reply_to_user,
    player_id = 0,
    battle_pid = undefined
}).


init(ReplyFun) ->
    State = #state{
        reply_to_user = ReplyFun
    },

    lager:info("Player created with pid ~p; state ~p",
               [self(), State]),

    {ok, State}.

handle_call({auth, Login, Pass}, _From, State) ->
    Res = pl_storage_srv:check_credentials(Login, Pass),

    case Res of
        {ok, PlayerId} ->
            {reply, ok, State#state{player_id = PlayerId}};

        error ->
            {reply, error, State}
    end;

handle_call({auth, Token}, _From, State) ->
    Res = pl_storage_srv:check_token(Token),

    case Res of
        {ok, PlayerId} ->
            {reply, ok, State#state{player_id = PlayerId}};

        error ->
            {reply, error, State}
    end;

handle_call({start_battle, {Currency, Bid}},
            _From,
            #state{player_id = PlayerId,
                   battle_pid = BattlePid} = State) ->
    AuthDone = player_authenticated(PlayerId),
    PlayerInBattle = battle_active(BattlePid),
    EnoughMoney = pl_storage_srv:check_enough_money(PlayerId, Currency, Bid),

    case {AuthDone, PlayerInBattle, EnoughMoney} of
        {true, false, true} ->
            pl_queue_srv:add_player(self(), PlayerId, {Currency, Bid}),
            {reply, ok, State};

        {false, _, _} ->
            {reply, {error, <<"USER IS NOT AUTHENTICATED\n">>}, State};

        {_, true, _} ->
            {reply, {error, <<"USER ALREADY IN THE BATTLE\n">>}, State};

        {_, _, false} ->
            {reply, {error, <<"YOU HAVE NOT ENOUGH MONEY\n">>}, State}
    end;

handle_call(get_id,
            _From,
            #state{player_id = PlayerId} = State) ->
    {reply, PlayerId, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(matching_in_progress, #state{reply_to_user = ReplyFun} = State) ->
    ReplyFun(<<"Searching the opponent...\n">>),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

player_authenticated(PlayerId) ->
    if
        PlayerId == 0 ->
            false;

        true ->
            true
    end.

battle_active(Pid) ->
    case Pid of
        BattlePid when is_pid(BattlePid) ->
            true;

        _ ->
            false
    end.