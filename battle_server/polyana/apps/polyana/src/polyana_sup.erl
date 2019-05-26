-module(polyana_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecifications = [
        #{
            id => pl_player_sup,
            start => {pl_player_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [pl_player_sup]
        },
       #{
           id => pl_storage_sup,
           start => {pl_storage_sup, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => supervisor,
           modules => [pl_storage_sup]
       },
        #{
            id => pl_matching_sup,
            start => {pl_matching_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [pl_matching_sup]
        },
        #{
            id => pl_battle_sup,
            start => {pl_battle_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [pl_battle_sup]
        }
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
