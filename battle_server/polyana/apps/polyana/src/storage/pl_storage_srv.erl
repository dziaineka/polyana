-module(pl_storage_srv).

-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, check_credentials/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_credentials(Login, Pass) ->
    gen_server:call(?MODULE, {check_credentials, Login, Pass}).

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
            State = #state{connection = Conn}) ->

    Query = ["select * from player where nickname = '", Login,
             "' and password = '", Pass, "'"],

    case epgsql:squery(Conn, Query) of
        {ok, _Columns, []} ->
            {reply, error, State};

        {ok, _Columns, [Player]} ->
            PlayerId = element(1, Player),
            {reply, {ok, PlayerId}, State};

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
