-module(pl_storage_srv).

-behaviour(gen_server).

%% API
-export([stop/1,
         start_link/0,
         check_credentials/2,
         check_token/1,
         get_rating/1,
         check_enough_money/3,
         exchange_currency/3,
         save_battle/3,
         save_event/3,
         save_transaction/4,
         save_winner/2,
         add_played_game/1,
         update_winrate/1,
         add_won_game/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection}).


stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_credentials(Login, Pass) ->
    gen_server:call(?MODULE, {check_credentials, Login, Pass}).

check_token(Token) ->
    gen_server:call(?MODULE, {check_token, Token}).

get_rating(PlayerId) ->
    gen_server:call(?MODULE, {get_rating, PlayerId}).

check_enough_money(PlayerId, CurrencyType, Bid) ->
    gen_server:call(?MODULE, {check_enough_money, PlayerId, CurrencyType, Bid}).

exchange_currency(From, To, Amount) ->
    gen_server:call(?MODULE, {exchange_currency, From, To, Amount}).

save_battle(CurrencyType, Bid, Players) ->
    gen_server:call(?MODULE, {save_battle, CurrencyType, Bid, Players}).

save_event(EventType, SourceId, PlayerId) ->
    gen_server:call(?MODULE, {save_event, EventType, SourceId, PlayerId}).

save_transaction(EventId, PlayerId, CurrencyType, Amount) ->
    gen_server:call(?MODULE, {save_transaction,
                              EventId,
                              PlayerId,
                              CurrencyType,
                              Amount}).

save_winner(BattleId, PlayerId) ->
    gen_server:call(?MODULE, {save_winner, BattleId, PlayerId}).

add_played_game(PlayerId) ->
    gen_server:cast(?MODULE, {add_played_game, PlayerId}).

update_winrate(PlayerId) ->
    gen_server:cast(?MODULE, {update_winrate, PlayerId}).

add_won_game(PlayerId) ->
    gen_server:cast(?MODULE, {add_won_game, PlayerId}).


init(_Args) ->
    {ok, User} = application:get_env(polyana, pguser),
    {ok, Password} = application:get_env(polyana, pgpassword),

    {ok, Conn} = epgsql:connect("localhost", User, Password, #{
        database => "polyana",
        port => 15432
    }),

    lager:info("db connected ~p ~p", [self(), Conn]),

    {ok, #state{connection=Conn}}.

handle_call(stop, _From, State) ->
    epgsql:close(State#state.connection),
    {stop, normal, stopped, State};

handle_call({check_credentials, Login, Pass},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["select * from polyana_player where nickname = '", Login,
             "' and password = md5('", Pass, "')"],

    make_auth_query(Conn, Query, State);

handle_call({check_token, Token},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["select * from polyana_player where token = '", Token,
             "' and token_expiration >= now()"],

    make_auth_query(Conn, Query, State);

handle_call({get_rating, PlayerId},
            _From,
            State = #state{connection = Conn}) ->
    Query = ["select winrate from polyana_player where id = ",
             binary_to_list(PlayerId)],

    case epgsql:squery(Conn, Query) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [{BinRating}]} ->
            {reply, {ok, binary_to_number(BinRating)}, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({check_enough_money, PlayerId, CurrencyType, Bid},
            _From,
            State = #state{connection = Conn}) ->

    Query = [
        "SELECT * FROM polyana_money ",
        "WHERE player_id = ", binary_to_list(PlayerId), " ",
        "AND currency_id = (SELECT id FROM polyana_currency WHERE type = '",
                            binary_to_list(CurrencyType), "') "
        "AND amount >= ", integer_to_list(Bid)
    ],

    case epgsql:squery(Conn, Query) of
        {ok, _Columns, []} ->
            {reply, false, State};

        {ok, _Columns, [_]} ->
            {reply, true, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, false, State}
    end;

handle_call({exchange_currency, From, To, Amount},
            _From,
            State = #state{connection = Conn}) ->
    {
        reply,
        round((Amount * get_rate(Conn, From)) / get_rate(Conn, To)),
        State
    };

handle_call({save_battle, CurrencyType, Bid, Players},
            _From,
            #state{connection = Conn} = State) ->
    QueryPlayerIds = get_array_for_query(Players),

    Query = ["INSERT INTO polyana_battle (currency_id, bid, participants, created) ",
             "VALUES ((SELECT id FROM polyana_currency WHERE type = $1), $2, $3, $4) RETURNING id"],

    Parameters = [binary_to_list(CurrencyType), Bid, QueryPlayerIds, erlang:timestamp()],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{BattleId}]} ->
            {reply, {ok, BattleId}, State}
    end;

handle_call({save_event, battle_start, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->

    Query = ["INSERT INTO polyana_event (player_id, type, source, created) ",
             "VALUES ($1, $2, $3, $4) RETURNING id"],

    Parameters = [binary_to_integer(PlayerId), "battle_start", BattleId, erlang:timestamp()],

    lager:warning("battle_start ~p", [Parameters]),

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{EventId}]} ->
            lager:warning("battle_start ok ~p", [EventId]),
            {reply, {ok, EventId}, State};

        Res -> lager:warning("battle_start fail ~p", [Res])
    end;

handle_call({save_event, battle_end, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO polyana_event (player_id, type, source, created) ",
             "VALUES ($1, $2, $3, $4) RETURNING id"],

    Parameters = [binary_to_integer(PlayerId), "battle_end", BattleId, erlang:timestamp()],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{EventId}]} ->
            {reply, {ok, EventId}, State}
    end;

handle_call({save_transaction, EventId, PlayerId, CurrencyType, Amount},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO polyana_transaction (event_id, player_id, currency_id, amount) ",
             "VALUES ($1, $2, (SELECT id FROM polyana_currency WHERE type = $3), $4) RETURNING id"],

    Parameters = [EventId, binary_to_integer(PlayerId), binary_to_list(CurrencyType), Amount],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{TransactionId}]} ->
            ok = update_money_balance(Conn, PlayerId, CurrencyType, Amount),
            {reply, {ok, TransactionId}, State}
    end;

handle_call({save_winner, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_battle SET winner_id = $1 WHERE id = $2"],
    Parameters = [binary_to_integer(PlayerId), BattleId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1} ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_played_game, PlayerId}, #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_player ",
             "SET played_battles = (",
             "(SELECT played_battles ",
                "FROM polyana_player ",
                "WHERE id = ", binary_to_list(PlayerId), ") + 1) ",
             "WHERE id = ", binary_to_list(PlayerId)],

    case epgsql:squery(Conn, Query) of
        {ok, 1} ->
            {noreply, State}
    end;

handle_cast({add_won_game, PlayerId}, #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_player ",
             "SET battles_won = (",
             "(SELECT battles_won ",
                 "FROM polyana_player ",
                 "WHERE id = ", binary_to_list(PlayerId), ") + 1) ",
             "WHERE id = ", binary_to_list(PlayerId)],

    case epgsql:squery(Conn, Query) of
        {ok, 1} ->
            {noreply, State}
    end;

handle_cast({update_winrate, PlayerId}, #state{connection = Conn} = State) ->
    case get_won_battles(Conn, PlayerId) of
        0 ->
            {noreply, State};

        _Amount ->
            Query = ["UPDATE polyana_player ",
                    "SET winrate = (",
                    "(SELECT cast(battles_won AS float) "
                        "FROM polyana_player ",
                        "WHERE id = ", binary_to_list(PlayerId),") / ",
                    "(SELECT played_battles ",
                        "FROM polyana_player ",
                        "WHERE id = ", binary_to_list(PlayerId), ")) ",
                    "WHERE id = ", binary_to_list(PlayerId)],

            case epgsql:squery(Conn, Query) of
                {ok, 1} ->
                    {noreply, State}
            end
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    epgsql:close(State#state.connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_won_battles(Connection, PlayerId) ->
    Query = ["select battles_won from polyana_player where id = ",
            binary_to_list(PlayerId)],

    case epgsql:squery(Connection, Query) of
        {ok, _Columns, [{BinGames}]} ->
            binary_to_integer(BinGames)
    end.

update_money_balance(Connection, PlayerId, CurrencyType, Amount) ->
    Query = [
        "UPDATE polyana_money ",
        "SET amount = ((SELECT amount FROM polyana_money ",
                       "WHERE player_id = ", binary_to_list(PlayerId),
                       " AND currency_id = ",
                            "(SELECT id FROM polyana_currency WHERE type = '",
                                binary_to_list(CurrencyType), "')) ",
                            " + ", integer_to_list(Amount), ") ",
        "WHERE player_id = ", binary_to_list(PlayerId),
        " AND currency_id = ", "(SELECT id FROM polyana_currency WHERE type = '",
                                            binary_to_list(CurrencyType), "')"],

    case epgsql:squery(Connection, Query) of
        {ok, 1} ->
            ok
    end.

get_array_for_query(Values) ->
    lists:foldl(
        fun (Value, Array) ->
            [binary_to_integer(Value) | Array]
        end,
        [],
        Values
    ).

make_auth_query(Connection, Query, State) ->
    case epgsql:squery(Connection, Query) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [Player]} ->
            PlayerId = element(1, Player),
            update_token(Connection, PlayerId),
            {reply, {ok, PlayerId}, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, error, State}
    end.

update_token(Connection, PlayerId) ->
    Query = ["select token from polyana_player where id = ", PlayerId,
             " and token_expiration >= now()"],

    case epgsql:squery(Connection, Query) of
        {ok, _Columns, []} ->
            create_token(Connection, PlayerId);

        {ok, _Columns, _} ->
            extend_token_lifetime(Connection, PlayerId)
    end.

get_new_token() ->
    quickrand:seed(),
    uuid:uuid_to_string(uuid:get_v4_urandom()).

create_token(Connection, PlayerId) ->
    Query = [
        "UPDATE polyana_player ",
        "SET token = '", get_new_token(), "', ",
        "token_expiration = timezone('UTC'::text, now()) + interval '1 hour'",
        "WHERE id = '", PlayerId, "'"
    ],

    {ok, _} = epgsql:squery(Connection, Query).

extend_token_lifetime(Connection, PlayerId) ->
    Query = [
        "UPDATE polyana_player ",
        "SET token_expiration = timezone('UTC'::text, now()) + interval '1 hour'",
        "WHERE id = '", PlayerId, "'"
    ],

    {ok, _} = epgsql:squery(Connection, Query).

get_rate(Connection, Currency) ->
    Query = ["SELECT rate FROM polyana_currency WHERE type = '",
             binary_to_list(Currency), "'"],

    {ok, _Columns, [{Rate}]} = epgsql:squery(Connection, Query),
    binary_to_number(Rate).

binary_to_number(Binary) ->
    StrNumber = binary_to_list(Binary),

    case string:to_float(StrNumber) of
        {error,no_float} -> list_to_integer(StrNumber);
        {Float,_Rest} -> Float
    end.