-module(polyana_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    start_ranch(),
    polyana_sup:start_link().


stop(_State) ->
    ok.


start_ranch() ->
    {ok, Port} = application:get_env(polyana, port),
    {ok, _} = ranch:start_listener(
        polyana_game_tcp_endpoint,
        ranch_tcp,
        [{port, Port}],
        pl_game_protocol,
        []
    ).