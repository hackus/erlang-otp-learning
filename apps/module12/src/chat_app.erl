%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2025 2:51 PM
%%%-------------------------------------------------------------------
-module(chat_app).
-author("hackus").

-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_Type, _Args) ->
  chat_sup:start_link().

stop(_State) ->
  ok.