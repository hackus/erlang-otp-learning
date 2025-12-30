%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 8:57 PM
%%%-------------------------------------------------------------------
-module(module10_2_sup).
-author("hackus").

-behaviour(supervisor).

%% Public API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  %% Start and register the supervisor locally
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callback
%%====================================================================

init([]) ->
  %% Child specifications
  %% Each child owns its ETS table
  %% If a child crashes, its ETS table is destroyed and recreated

  EtsSafeStore =
    {ets_safe_store,
      {ets_safe_store, start_link, []},
      permanent,
      5000,
      worker,
      [ets_safe_store]},

  EtsFastStore =
    {ets_fast_store,
      {ets_fast_store, start_link, []},
      permanent,
      5000,
      worker,
      [ets_fast_store]},

  EtsCounterStore =
    {ets_counter_store,
      {ets_counter_store, start_link, []},
      permanent,
      5000,
      worker,
      [ets_counter_store]},

  EtsAdvancedCounterStore =
    {ets_advanced_counter_store,
      {ets_advanced_counter_store, start_link, []},
      permanent,
      5000,
      worker,
      [ets_advanced_counter_store]},

  Children = [
    EtsSafeStore,
    EtsFastStore,
    EtsCounterStore,
    EtsAdvancedCounterStore
  ],

  %% Supervision strategy:
  %% one_for_one  → crash of one store does NOT affect others
  %% 5            → max restarts
  %% 10           → in seconds
  {ok, {{one_for_one, 5, 10}, Children}}.
