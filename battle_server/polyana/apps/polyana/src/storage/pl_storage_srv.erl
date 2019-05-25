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
         save_winner/2]).

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
    Query = ["select * from player where nickname = '", Login,
             "' and password = '", Pass, "'"],

    make_auth_query(Conn, Query, State);

handle_call({check_token, Token},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["select * from player where token = '", Token,
             "' and token_expiration >= now()"],

    make_auth_query(Conn, Query, State);

handle_call({get_rating, PlayerId},
            _From,
            State = #state{connection = Conn}) ->
    Query = ["select winrate from player where id = ",
             binary_to_list(PlayerId)],

    case epgsql:squery(Conn, Query) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [{BinRating}]} ->
            {reply, {ok, binary_to_integer(BinRating)}, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({check_enough_money, PlayerId, CurrencyType, Bid},
            _From,
            State = #state{connection = Conn}) ->

    Query = [
        "SELECT * FROM money ",
        "WHERE player_id = ", binary_to_list(PlayerId), " ",
        "AND currency_id = (SELECT id FROM currency WHERE type = '",
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

    Query = ["INSERT INTO battle (currency_id, bid, participants) ",
             "VALUES ((SELECT id FROM currency WHERE type = '",
                            binary_to_list(CurrencyType), "'), ",
                      integer_to_list(Bid), ", ",
                      QueryPlayerIds, ") ",
             "RETURNING id"],

    case epgsql:squery(Conn, Query) of
        {ok, 1, _Columns, [{BattleId}]} ->
            {reply, {ok, BattleId}, State}
    end;

handle_call({save_event, battle_start, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO event (player_id, type, source) ",
             "VALUES ('", binary_to_list(PlayerId), "', ",
                      "'battle_start', ",
                      "'", binary_to_list(BattleId), "') ",
             "RETURNING id"],

    case epgsql:squery(Conn, Query) of
        {ok, 1, _Columns, [{EventId}]} ->
            {reply, {ok, EventId}, State}
    end;

handle_call({save_transaction, EventId, PlayerId, CurrencyType, Amount},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO transaction (event_id, player_id, currency_id, amount) ",
             "VALUES ('", binary_to_list(EventId), "', ",
                      "'", binary_to_list(PlayerId), "', ",
                      "(SELECT id FROM currency WHERE type = '",
                            binary_to_list(CurrencyType), "'), ",
                      integer_to_list(Amount), ") ",
             "RETURNING id"],

    case epgsql:squery(Conn, Query) of
        {ok, 1, _Columns, [{TransactionId}]} ->
            ok = update_money_balance(Conn, PlayerId, CurrencyType, Amount),
            {reply, {ok, TransactionId}, State}
    end;

handle_call({save_winner, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["UPDATE battle SET winner = ", binary_to_list(PlayerId),
             " WHERE id = ", binary_to_list(BattleId)],

    case epgsql:squery(Conn, Query) of
        {ok, 1} ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    epgsql:close(State#state.connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


update_money_balance(Connection, PlayerId, CurrencyType, Amount) ->
    Query = [
        "UPDATE money ",
        "SET amount = ((SELECT amount FROM money ",
                       "WHERE player_id = ", binary_to_list(PlayerId),
                       " AND currency_id = ",
                            "(SELECT id FROM currency WHERE type = '",
                                binary_to_list(CurrencyType), "')) ",
                            " + ", integer_to_list(Amount), ") ",
        "WHERE player_id = ", binary_to_list(PlayerId),
        " AND currency_id = ", "(SELECT id FROM currency WHERE type = '",
                                            binary_to_list(CurrencyType), "')"],

    case epgsql:squery(Connection, Query) of
        {ok, 1} ->
            ok
    end.

get_array_for_query(Values) ->
    ArrayForQuery = lists:foldl(
        fun (Value, Array) ->
            lists:append(Array, [binary_to_list(Value)])
        end,
        [],
        Values
    ),

    lists:append([["'{"], lists:join(", ", ArrayForQuery), ["}'"]]).

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
    Query = ["select token from player where id = ", PlayerId,
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
        "UPDATE player ",
        "SET token = '", get_new_token(), "', ",
        "token_expiration = timezone('UTC'::text, now()) + interval '1 hour'",
        "WHERE id = '", PlayerId, "'"
    ],

    {ok, _} = epgsql:squery(Connection, Query).

extend_token_lifetime(Connection, PlayerId) ->
    Query = [
        "UPDATE player ",
        "SET token_expiration = timezone('UTC'::text, now()) + interval '1 hour'",
        "WHERE id = '", PlayerId, "'"
    ],

    {ok, _} = epgsql:squery(Connection, Query).

get_rate(Connection, Currency) ->
    Query = ["SELECT rate FROM currency WHERE type = '",
             binary_to_list(Currency), "'"],

    {ok, _Columns, [{Rate}]} = epgsql:squery(Connection, Query),

    try binary_to_float(Rate) of
        FloatRate ->
            FloatRate
    catch
        _:_:_ ->
            binary_to_integer(Rate)
    end.