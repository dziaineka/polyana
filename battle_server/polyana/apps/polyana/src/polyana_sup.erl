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
        % #{
        %     id => pl_player_storage,
        %     start => {pl_player_storage, start_link, []},
        %     restart => permanent,
        %     shutdown => 2000,
        %     type => worker,
        %     modules => [pl_player_storage]
        % },
        #{
            id => pl_player_sup,
            start => {pl_player_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [pl_player_sup]
        }
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
