%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:01 PM
%%%-------------------------------------------------------------------
-module(module11_2_sup).
-author("hackus").
-behaviour(supervisor).


%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Dispatch = router:dispatch(),

  %% Correct Cowboy startup for Cowboy 2.14
  {ok, _Pid} =
    cowboy:start_clear(
      http_listener,
      [{port, 8080}],
      #{env => #{dispatch => Dispatch}}
    ),

  users_store:init(),

  %% Cowboy manages its own acceptors internally
  {ok, {{one_for_one, 5, 10}, []}}.