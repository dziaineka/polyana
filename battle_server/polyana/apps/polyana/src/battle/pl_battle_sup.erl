-module(pl_battle_sup).
-author("kalinin").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
  SupervisorSpecification = #{
    strategy => simple_one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [
      #{
        id => pl_battle,
        start => {pl_battle, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [pl_battle]
      }
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
