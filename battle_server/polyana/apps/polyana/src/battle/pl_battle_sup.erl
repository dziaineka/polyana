%%%-------------------------------------------------------------------
%%% @author kalinin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Май 2019 13:40
%%%-------------------------------------------------------------------
-module(pl_battle_sup).
-author("kalinin").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart =
    temporary,
%%    permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {pl_battle, {pl_battle, start_link, []},
    Restart, Shutdown, Type, [pl_battle]},
%%  BChild = {meadow_player_srv, {meadow_battle, start_link, []},
%%    Restart, Shutdown, Type, [meadow_player_srv]},

  {ok, {SupFlags, [AChild
%%    , BChild
  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
