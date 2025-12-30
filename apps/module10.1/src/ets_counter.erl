%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 9:49 AM
%%%-------------------------------------------------------------------
-module(ets_counter).
-author("hackus").

%% API
-export([start/0, inc/0, value/0]).

start() ->
  %% create if not exists
  case ets:info(counter) of
    undefined ->
      ets:new(counter, [named_table, set, public]),
      ets:insert(counter, {count, 0});
    _ -> ok
  end,
  ok.

inc() ->
  ets:update_counter(counter, count, 1).

value() ->
  [{count, V}] = ets:lookup(counter, count),
  V.
