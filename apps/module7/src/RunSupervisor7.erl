%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2025 11:26 PM
%%%-------------------------------------------------------------------
-module('RunSupervisor7').
-author("hackus").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %% Supervisor flags
  SupFlags = #{
    strategy  => one_for_one,  % <── here is your strategy
    intensity => 5,            % max 5 restarts
    period    => 10            % within 10 seconds
  },

  %% Child specification (one example worker)
  ChildSpecs = [
    #{
      id       => 'WorkerGenServer',
      start    => {'WorkerGenServer', start_link, []},
      restart  => permanent,  % always restart if it crashes
      shutdown => 5000,
      type     => worker,
      modules  => ['WorkerGenServer']
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
