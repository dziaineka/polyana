-module(pl_game_protocol).
-behaviour(ranch_protocol).


-export([start_link/4, init/3]).


-record(state, {
    transport,
    socket,
    player_srv
}).


start_link(Ref, _Socket, Transport, Opt)->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opt]),
    {ok, Pid}.


init(Ref, Trasport, _Opt = [])->
    {ok, Socket} = ranch:handshake(Ref),
    lager:info("new connection, create player"),
    {ok, Pid} = supervisor:start_child(meadow_players_sup, [Socket]),
    Res = gen_tcp:controlling_process(Socket, Pid),
    lager:info("controlling process res:~p", [Res]),
    State = #state{
        transport = Trasport,
        socket = Socket,
        player_srv = Pid
    },
    loop(State).


loop(#state{transport = Transport, socket = Socket, player_srv = PlayerPid} = State)->
    {ok, Raw_Timeout} = application:get_env(meadow, client_disconnect_timeout),
    Timeout = Raw_Timeout*60*1000,
    case Transport:recv(Socket, 0, Timeout) of
        {ok, <<"PING", _/binary>>} ->
            Reply = handle_ping(),
            Transport:send(Socket, Reply),
            loop(State);
        {ok, <<"AUTH ", LoginPass/binary>>} ->
            case handle_auth(PlayerPid, LoginPass) of
                {ok, Reply} ->  Transport:send(Socket, Reply),
                    loop(State);
                {error, Reply} -> Transport:send(Socket, Reply),
                    loop(State)
            end;
        {ok, <<"START", _/binary>>} ->
            Raw_Reply = handle_command({get_name, start_game}, PlayerPid),
            multicast(Raw_Reply, PlayerPid, Transport, Socket, State);
        {ok, <<"STOP", _/binary>>} ->
            pl_player_srv:stop(PlayerPid),
            ok = Transport:close(Socket);
        {ok, <<"U", _/binary>>} ->
            Raw_Reply = handle_command({get_name, {get_battle_pid, {move, up}}}, PlayerPid),
            multicast(Raw_Reply, PlayerPid, Transport, Socket, State);
        {ok, <<"D", _/binary>>} ->
            Raw_Reply = handle_command({get_name, {get_battle_pid, {move, down}}}, PlayerPid),
            multicast(Raw_Reply, PlayerPid, Transport, Socket, State);
        {ok, <<"L", _/binary>>} ->
            Raw_Reply = handle_command({get_name, {get_battle_pid, {move, left}}}, PlayerPid),
            multicast(Raw_Reply, PlayerPid, Transport, Socket, State);
        {ok, <<"R", _/binary>>} ->
            Raw_Reply = handle_command({get_name, {get_battle_pid, {move, right}}}, PlayerPid),
            multicast(Raw_Reply, PlayerPid, Transport, Socket, State);
        {ok, UnknownData} ->
            Reply = <<"INVALID QUERY\n">>,
            lager:warning("Invalid query: ~p", [UnknownData]),
            Transport:send(Socket, Reply),
            loop(State);
        {error, Error}->
            lager:info("close, connection, ~p stop player", [Error]),
            pl_player_srv:stop(PlayerPid),
            ok = Transport:close(Socket)
    end.


handle_ping() -> <<"PONG\n">>.


handle_auth(PlayerPid, LoginPass) ->
    [Login, Pass | _] = binary:split(LoginPass, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
    lager:info("got login ~p pass ~p", [Login, Pass]),
    Res = pl_player_srv:auth(PlayerPid, Login, Pass),% надо проверить функцию для проверки аутентификации
    case Res of
        ok -> {ok , <<"success\n">>};
        error ->
            {error, <<"Incorrect login or password\n">>}
    end.


multicast(Raw_Reply, Pid, Transport, Socket, State) ->
%% функция для отправки сообщение либо всем участникам игры, либо одному в зависимости от ситуации
    case Raw_Reply of
        {win, Reply} -> {ok, BattlePid}=pl_player_srv:get_battle_pid(Pid),%для получения баттл пид
            Players = pl_battle:get_players(BattlePid),
            lists:foreach(fun(PlayerSrv) ->
                Player_Socket = pl_player_srv:get_socket(PlayerSrv),%для получения сокета
                PlayerSrv ! close_room, % в pl_player_srv в handle_info нужна слеудющая функция
                %handle_info(close_room, State) ->
%%                {noreply, State#state{battle_pid = none}};
                Transport:send(Player_Socket, Reply) end, Players),
            pl_battle:stop(BattlePid),
            loop(State);
        {multi, Reply} ->
            {ok, BattlePid}=pl_player_srv:get_battle_pid(Pid), %для получения баттл пид
            Players = pl_battle:get_players(BattlePid),
            lists:foreach(fun(PlayerSrv) ->
                Player_Socket = pl_player_srv:get_socket(PlayerSrv), %для получения сокета
                Transport:send(Player_Socket, Reply) end, Players),
            loop(State);
        {single, Reply} -> Transport:send(Socket, Reply),
            loop(State);
        Reply->Transport:send(Socket, Reply),loop(State)
    end.


handle_command({get_name, Enc}, PlayerPid)->
    case pl_players_srv:nickname(PlayerPid) of %код для проверки аутентификации
        {ok, _} -> handle_command(Enc, PlayerPid);
        {error, not_auth} -> <<"Login first, please\n">>
    end;

handle_command({get_battle_pid, Enc}, Pid)->
    case meadow_players_srv:get_battle_pid(Pid) of % код для получение battlepid потока
        {ok, BattlePid} -> handle_command(Enc, {BattlePid, Pid});
        {error, not_game} -> <<"First, start game\n">>
    end;

handle_command(start_game, PlayerPid)->
    PlayerID = pl_players_srv:nickname(PlayerPid), %код для получения PlayerID, для последуюшей
                                                        % работы в матчмейкинга
    pl_queue_srv:add_player(PlayerPid, PlayerID);

handle_command({move, Direction},{BattlePid, PlayerPid}) ->
    {Flag, Msg, Raw_Field}= pl_battle:move(BattlePid, PlayerPid, Direction),
    Field = list_to_binary(Raw_Field),
    {Flag, <<Field/binary, Msg/binary, "\n">>}.
