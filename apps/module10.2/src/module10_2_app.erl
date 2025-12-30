%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2025 9:06 PM
%%%-------------------------------------------------------------------
-module(module10_2_app).
-author("hackus").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
  %% Start the top-level supervisor
  module10_2_sup:start_link().

stop(_State) ->
  %% Called when application stops
  %% Cleanup can be added here if needed
  ok.
