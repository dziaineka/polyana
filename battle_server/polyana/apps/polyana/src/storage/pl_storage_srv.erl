-module(pl_storage_srv).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, check_credentials/2, check_token/1,
         get_rating/1]).

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
             integer_to_list(PlayerId)],

    case epgsql:squery(Conn, Query) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [{BinRating}]} ->
            {reply, {ok, binary_to_integer(BinRating)}, State};

        Unhandled ->
            lager:warning("Unexpected query result: ~p", [Unhandled]),
            {reply, error, State}
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
