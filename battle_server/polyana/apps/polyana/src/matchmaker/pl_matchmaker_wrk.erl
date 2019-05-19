-module(pl_matchmaker_wrk).

-export([start_link/0]).
-export([init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    % probably have to initialize some state, but it is not clear so far

    loop().

loop() ->
    % matching
    lager:info("matching process"),
    timer:sleep(30000),

    loop().