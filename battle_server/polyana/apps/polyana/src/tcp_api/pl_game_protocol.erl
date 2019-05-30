-module(pl_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-record(state, {
    transport,
    socket,
    player_srv
}).

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),

    lager:info("new connection, create player"),

    ReplyToUser = fun
        (Reply) ->
            Transport:send(Socket, Reply)
    end,

    {ok, PlayerSrv} = supervisor:start_child(pl_player_sup, [ReplyToUser]),
    Res = gen_tcp:controlling_process(Socket, PlayerSrv),
    lager:info("controlling_process res:~p", [Res]),

    State = #state{
        transport = Transport,
        socket = Socket,
        player_srv = PlayerSrv
    },

    loop(State).


loop(#state{socket = Socket,
            transport = Transport,
            player_srv = PlayerSrv} = State) ->
    {ok, ClientDisconnectTimeoutMin} = application:get_env(
                                            polyana,
                                            client_disconnect_timeout),

    ClientDisconnectTimeoutMilliSec = ClientDisconnectTimeoutMin * 60 * 1000,

    case Transport:recv(Socket, 0, ClientDisconnectTimeoutMilliSec) of
        {ok, <<"PING", _/binary>>} ->
            Reply = handle_ping(),
            Transport:send(Socket, Reply),
            loop(State);

        {ok, <<"AUTH ", LoginPass/binary>>} ->
            case handle_auth(PlayerSrv, LoginPass) of
                {ok, Reply} ->
                    Transport:send(Socket, Reply),
                    loop(State);
                {error, Reply} ->
                    Transport:send(Socket, Reply),
                    loop(State)
            end;

        {ok, <<"BATTLE ", Bid/binary>>} ->
            Reply = handle_battle(PlayerSrv, Bid, head_to_head),
            Transport:send(Socket, Reply),
            loop(State);

        {ok, <<"ROYALE ", Bid/binary>>} ->
            Reply = handle_battle(PlayerSrv, Bid, battle_royale),
            Transport:send(Socket, Reply),
            loop(State);

        {ok, <<"STOP", _/binary>>} ->
            pl_player_srv:stop(PlayerSrv),
            ok = Transport:close(Socket);

        {ok, <<"w", _/binary>>} ->
            pl_player_srv:move(up, PlayerSrv),
            loop(State);

        {ok, <<"s", _/binary>>} ->
            pl_player_srv:move(down, PlayerSrv),
            loop(State);

        {ok, <<"a", _/binary>>} ->
            pl_player_srv:move(left, PlayerSrv),
            loop(State);

        {ok, <<"d", _/binary>>} ->
            pl_player_srv:move(right, PlayerSrv),
            loop(State);

        {error, Error} ->
            lager:info("close connection, ~p stop player", [Error]),
            pl_player_srv:stop(PlayerSrv),
            ok = Transport:close(Socket);

        {ok, UnknownData} ->
            Reply = <<"INVALID QUERY\n">>,
            lager:warning("Invalid Query:~p", [UnknownData]),
            Transport:send(Socket, Reply),
            loop(State)
    end.

handle_ping() -> <<"PONG\n">>.

handle_auth(PlayerSrv, LoginPass) ->
    FilteredInput = lists:filter(
        fun
            (ByteString) when ByteString == <<>> ->
                false;

            (_) ->
                true
        end,
        binary:split(LoginPass, [<<" ">>, <<"\n">>, <<"\r">>], [global])
    ),

    case FilteredInput of
        [Login, Pass | _] ->
            lager:info("try to login with Login:~p Pass:~p", [Login, Pass]),
            Res = pl_player_srv:auth(PlayerSrv, Login, Pass);

        [Token] ->
            lager:info("try to login with Token:~p", [Token]),
            Res = pl_player_srv:auth(PlayerSrv, Token)
    end,

    case Res of
        ok -> {ok, <<"SUCCESS\n">>};
        error -> {error, <<"AUTH FAILED\n">>}
    end.

handle_battle(PlayerSrv, Parameters, BattleType) ->
    StrBid = binary:replace(binary:replace(Parameters, <<"\r\n">>, <<>>),
                            <<" ">>,
                            <<>>),

    case string:to_integer(StrBid) of
        {error, _} ->
            <<"INVALID PARAMETERS\n">>;

        {Bid, Currency} when
                (Currency == <<"GOLD">>) or (Currency == <<"SILVER">>) -> % прибить гвоздями - это так
            lager:info("battle player ~p", [PlayerSrv]),

            Res = pl_player_srv:start_battle(PlayerSrv,
                                             {Currency, Bid, BattleType}),

            case Res of
                ok -> <<"TO BATTLE!\n">>;
                {error, Reason} -> Reason
            end;

        _ ->
            lager:warning("unhandled ~p", [StrBid]),
            <<"INVALID PARAMETERS\n">>
    end.
