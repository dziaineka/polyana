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
         add_won_game/1,
         get_played_battles/1,
         get_won_battles/1,
         check_achievement_ownership/2,
         save_achievement/2]).

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

get_played_battles(PlayerId) ->
    gen_server:call(?MODULE, {get_played_battles, PlayerId}).

get_won_battles(PlayerId) ->
    gen_server:call(?MODULE, {get_won_battles, PlayerId}).

check_achievement_ownership(PlayerId, AchievementType) ->
    gen_server:call(?MODULE,
                    {check_achievement_ownership, PlayerId, AchievementType}).

save_achievement(PlayerId, AchievementType) ->
    gen_server:call(?MODULE, {save_achievement, PlayerId, AchievementType}).

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
    Query = ["SELECT * FROM polyana_player ",
             "WHERE nickname = $1 ",
             "   AND password = md5($2)"],

    Parameters = [Login, Pass],

    make_auth_query(Conn, Query, Parameters, State);

handle_call({check_token, Token},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["SELECT * FROM polyana_player ",
             "WHERE token = $1",
             "   AND token_expiration >= now()"],

    Parameters = [Token],

    make_auth_query(Conn, Query, Parameters, State);

handle_call({get_rating, PlayerId},
            _From,
            State = #state{connection = Conn}) ->
    Query = ["SELECT winrate FROM polyana_player WHERE id = $1"],
    Parameters = [PlayerId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [{BinRating}]} ->
            {reply, {ok, BinRating}, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({check_enough_money, PlayerId, CurrencyType, Bid},
            _From,
            State = #state{connection = Conn}) ->

    Query = [
        "SELECT * FROM polyana_money ",
        "WHERE player_id = $1 ",
        "AND currency_id = (SELECT id FROM polyana_currency WHERE type = $2) "
        "AND amount >= $3"
    ],

    if
        Bid > 2147483647 -> % больше, чем постгресный integer
            NewBid = 2147483647;

        true ->
            NewBid = Bid
    end,

    Parameters = [PlayerId, binary_to_list(CurrencyType), NewBid],

    case epgsql:equery(Conn, Query, Parameters) of
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
    Query = ["INSERT INTO polyana_battle (currency_id, bid, participants, created) ",
             "VALUES ((SELECT id FROM polyana_currency WHERE type = $1), $2, $3, $4) RETURNING id"],

    Parameters = [binary_to_list(CurrencyType), Bid, Players, erlang:timestamp()],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{BattleId}]} ->
            {reply, {ok, BattleId}, State}
    end;

handle_call({save_event, EventType, Source, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO polyana_event (player_id, type, source, created) ",
             "VALUES ($1, $2, $3, $4) RETURNING id"],

    Parameters = [PlayerId,
                  atom_to_list(EventType),
                  Source,
                  erlang:timestamp()],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{EventId}]} ->
            {reply, {ok, EventId}, State}
    end;

handle_call({save_transaction, EventId, PlayerId, CurrencyType, Amount},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO polyana_transaction (event_id, player_id, currency_id, amount) ",
             "VALUES ($1, $2, (SELECT id FROM polyana_currency WHERE type = $3), $4) RETURNING id"],

    Parameters = [EventId, PlayerId, binary_to_list(CurrencyType), Amount],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{TransactionId}]} ->
            ok = update_money_balance(Conn, PlayerId, CurrencyType, Amount),
            {reply, {ok, TransactionId}, State}
    end;

handle_call({save_winner, BattleId, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_battle SET winner_id = $1 WHERE id = $2"],
    Parameters = [PlayerId, BattleId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1} ->
            {reply, ok, State}
    end;

handle_call({get_played_battles, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["SELECT count(id) FROM polyana_battle ",
             "WHERE $1 = ANY(participants)"],

    Parameters = [PlayerId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, _Columns, []} ->
            {reply, {ok, 0}, State};

        {ok, _Columns, [{Count}]} ->
            {reply, {ok, Count}, State};

        Unhandled ->
            lager:warning("Unexpected query result 1: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({get_won_battles, PlayerId},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["SELECT count(id) FROM polyana_battle ",
             "WHERE winner_id = $1"],

    Parameters = [PlayerId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, _Columns, []} ->
            {reply, {ok, 0}, State};

        {ok, _Columns, [{Count}]} ->
            {reply, {ok, Count}, State};

        Unhandled ->
            lager:warning("Unexpected query result 1: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({check_achievement_ownership, PlayerId, AchievementType},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["SELECT id FROM polyana_achievement ",
             "WHERE player_id = $1 AND type = $2"],

    Parameters = [PlayerId, atom_to_list(AchievementType)],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, _Columns, []} ->
            {reply, false, State};

        {ok, _Columns, [_]} ->
            {reply, true, State};

        Unhandled ->
            lager:warning("Unexpected query result 2: ~p", [Unhandled]),
            {reply, error, State}
    end;

handle_call({save_achievement, PlayerId, AchievementType},
            _From,
            #state{connection = Conn} = State) ->
    Query = ["INSERT INTO polyana_achievement (type, player_id, created) ",
             "VALUES ($1, $2, $3) RETURNING id"],

    Parameters = [atom_to_list(AchievementType), PlayerId, erlang:timestamp()],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1, _Columns, [{AchievementId}]} ->
            {reply, {ok, AchievementId}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_played_game, PlayerId}, #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_player ",
             "SET played_battles = (",
             "(SELECT played_battles ",
                "FROM polyana_player ",
                "WHERE id = $1) + 1) ",
             "WHERE id = $1"],

    Parameters = [PlayerId],

    case epgsql:equery(Conn, Query, Parameters) of
        {ok, 1} ->
            {noreply, State}
    end;

handle_cast({add_won_game, PlayerId}, #state{connection = Conn} = State) ->
    Query = ["UPDATE polyana_player ",
             "SET battles_won = (",
             "(SELECT battles_won ",
                 "FROM polyana_player ",
                 "WHERE id = $1) + 1) ",
             "WHERE id = $1"],

    Parameters = [PlayerId],

    case epgsql:equery(Conn, Query, Parameters) of
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
                        "WHERE id = $1) / ",
                    "(SELECT played_battles ",
                        "FROM polyana_player ",
                        "WHERE id = $1)) ",
                    "WHERE id = $1"],

            Parameters = [PlayerId],

            case epgsql:equery(Conn, Query, Parameters) of
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
    Query = ["SELECT battles_won FROM polyana_player WHERE id = $1"],
    Parameters = [PlayerId],

    case epgsql:equery(Connection, Query, Parameters) of
        {ok, _Columns, [{WonBattles}]} ->
            WonBattles
    end.

update_money_balance(Connection, PlayerId, CurrencyType, Amount) ->
    Query = [
        "UPDATE polyana_money ",
        "SET amount = ((SELECT amount FROM polyana_money ",
                       "WHERE player_id = $1",
                       " AND currency_id = ",
                            "(SELECT id FROM polyana_currency WHERE type = $2)) ",
                            " + $3) ",
        "WHERE player_id = $1",
        " AND currency_id = (SELECT id FROM polyana_currency WHERE type = $2)"],

    Parameters = [PlayerId, binary_to_list(CurrencyType), Amount],

    case epgsql:equery(Connection, Query, Parameters) of
        {ok, 1} ->
            ok
    end.

make_auth_query(Connection, Query, Parameters, State) ->
    case epgsql:equery(Connection, Query, Parameters) of
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
    Query = ["SELECT token FROM polyana_player WHERE id = $1",
             "  AND token_expiration >= now()"],

    Parameters = [PlayerId],

    case epgsql:equery(Connection, Query, Parameters) of
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
        "SET token = $1, ",
        "token_expiration = timezone('UTC'::text, now()) + interval '1 hour' ",
        "WHERE id = $2"
    ],

    Parameters = [get_new_token(), PlayerId],

    {ok, _} = epgsql:equery(Connection, Query, Parameters).

extend_token_lifetime(Connection, PlayerId) ->
    Query = [
        "UPDATE polyana_player ",
        "SET token_expiration = timezone('UTC'::text, now()) + interval '1 hour' ",
        "WHERE id = $1"
    ],

    Parameters = [PlayerId],

    {ok, _} = epgsql:equery(Connection, Query, Parameters).

get_rate(Connection, CurrencyType) ->
    Query = ["SELECT rate FROM polyana_currency WHERE type = $1"],
    Parameters = [binary_to_list(CurrencyType)],
    {ok, _Columns, [{Rate}]} = epgsql:equery(Connection, Query, Parameters),
    Rate.