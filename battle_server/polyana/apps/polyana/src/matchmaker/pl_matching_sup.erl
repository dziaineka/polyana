-module(pl_matching_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => rest_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => pl_queue_srv,
            start => {pl_queue_srv, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [pl_queue_srv]
        },
        #{
            id => pl_matchmaker_wrk,
            start => {pl_matchmaker_wrk, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [pl_matchmaker_wrk]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
