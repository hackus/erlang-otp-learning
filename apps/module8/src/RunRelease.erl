%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2025 8:13 PM
%%%-------------------------------------------------------------------
-module('RunRelease').
-author("hackus").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  io:format("Hello App starting~n"),
  'RunSupervisor':start_link().

stop(_State) ->
  io:format("Hello App stopping~n"),
  ok.