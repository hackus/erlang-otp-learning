%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 9:47 AM
%%%-------------------------------------------------------------------
-module(hw_ets).
-author("hackus").

%% API
-export([start/0]).

start() ->
  ets:new(my_table, [set, named_table, public]),
  io:format("ETS table created~n").

