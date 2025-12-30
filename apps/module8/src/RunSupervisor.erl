%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2025 8:15 PM
%%%-------------------------------------------------------------------
-module('RunSupervisor').
-author("hackus").
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, 'RunSupervisor'}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 1, 5},
    [
      {'RunWorker',
        {'RunWorker', start_link, []},
        permanent,
        5000,
        worker,
        ['RunWorker']}
    ]}}.
