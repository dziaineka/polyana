-module(pl_player_srv).
-behavior(gen_server).

-export([start_link/1, auth/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

auth(PlayerSrv, Login, Pass) ->
    gen_server:call(PlayerSrv, {auth, Login, Pass}).

stop(PlayerSrv) ->
    gen_server:call(PlayerSrv, stop).


%%% gen_server API

-record(state, {
    socket :: pid(),
    storage :: pid()
}).

init(Socket) ->
    {ok, StorageSrv} = supervisor:start_child(pl_storage_sup, []),

    State = #state{
        socket = Socket,
        storage = StorageSrv
    },

    lager:info("Player created with pid ~p; socket ~p; storage ~p",
               [self(), State, StorageSrv]),

    {ok, State}.

handle_call({auth, _Login, _Pass}, _From, State) ->
    %% go to database
    {reply, ok, State};

handle_call({auth, _Login, _Pass}, _From, State) ->
    {reply, error, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
