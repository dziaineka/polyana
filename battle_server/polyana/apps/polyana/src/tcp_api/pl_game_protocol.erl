-module(pl_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4, init/3]).
-export([handle_get_players/0]).

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
            meadow_battle:stop(PlayerPid),
            ok = Transport:close(Socket)
    end.


handle_ping() -> <<"PONG\n">>.

handle_auth(PlayerPid, LoginPass) ->
    [Login, Pass | _] = binary:split(LoginPass, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
    lager:info("got login ~p pass ~p", [Login, Pass]),
    Res = pl_player_srv:auth(PlayerPid, Login, Pass),
    case Res of
        ok -> {ok , <<"success\n">>};
        error ->
            {error, <<"Incorrect login or password\n">>}
    end.

multicast(Raw_Reply, Pid, Transport, Socket, State) ->
%% функция для отправки сообщение либо всем участникам игры, либо одному в зависимости от ситуации
    case Raw_Reply of
        {multi, Reply} ->
            {ok, BattlePid}=meadow_players_srv:get_battle_pid(Pid),
            Players = meadow_battle:get_players(BattlePid),
            lists:foreach(fun(PlayerSrv) ->
                Player_Socket = meadow_players_srv:get_socket(PlayerSrv),
                Transport:send(Player_Socket, Reply) end, Players),
            loop(State);
        {single, Reply} -> Transport:send(Socket, Reply),
            loop(State);
        Reply->Transport:send(Socket, Reply),loop(State)
    end.



handle_command({get_name, Enc}, PlayerPid)->
    case pl_players_srv:nickname(PlayerPid) of
        {ok, _} -> handle_command(Enc, PlayerPid);
        {error, not_auth} -> <<"Login first, please\n">>
    end;
handle_command({get_battle_pid, Enc}, Pid)->
    case meadow_players_srv:get_battle_pid(Pid) of % код для получение battlepid потока
        {ok, BattlePid} -> handle_command(Enc, {BattlePid, Pid});
        {error, not_game} -> <<"First, start game\n">>
    end;
handle_command(start_game, PlayerPid)->
    PlayerID = pl_players_srv:nickname(PlayerPid),
    pl_queue_srv:add_player(PlayerPid, PlayerID);
handle_command({move, Direction},{BattlePid, PlayerPid}) ->

    {Flag, Msg, Raw_Field, {M, _}}= pl_battle:move(BattlePid, PlayerPid, Direction),

    Field= new_field(Raw_Field, M),
    Field1 = maps:to_list(Field),
    Field2 = lists:keysort(1, Field1),
    V1 = lists:map(fun ({_K, V})-> V end, Field2),
%%    V = maps:values(Field),
    A = list_to_binary(V1),

    {Flag, <<A/binary, Msg/binary, "\n">>}.


new_field(A, -1)-> A;
new_field(A, M)-> new_field(A#{{M,a} => <<"\n">>}, M-1).