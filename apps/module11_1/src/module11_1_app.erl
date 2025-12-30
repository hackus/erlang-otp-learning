%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 12:00 PM
%%%-------------------------------------------------------------------
-module(module11_1_app).
-author("hackus").
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_Type, _Args) ->
  module11_1_sup:start_link().

stop(_State) ->
  ok.