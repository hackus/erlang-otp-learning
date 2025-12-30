%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:05 PM
%%%-------------------------------------------------------------------
-module(router).
-author("hackus").

%% API
-export([dispatch/0]).

dispatch() ->
  cowboy_router:compile([
    {'_', [
      {"/health", health_handler, []},
      {"/echo",   echo_handler,   []},
      {"/users/:id", users_handler, []}
    ]}
  ]).
