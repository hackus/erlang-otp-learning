%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2025 1:17 PM
%%%-------------------------------------------------------------------
-module('RunSupervisor7App').
-author("hackus").

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  'RunSupervisor7':start_link().

stop(_State) ->
  ok.