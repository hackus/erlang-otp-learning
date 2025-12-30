%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2025 8:19 PM
%%%-------------------------------------------------------------------
-module('Concurency').
-author("hackus").

%% API
-export([start/0,loop/0]).


start() ->
  %% initialize counter in process dictionary
  spawn(?MODULE, loop, []).

loop() ->
  receive
    {msg, Text} ->
      Count = getCounter(counter),
      NewCount = Count + 1,
      putCounter(counter, NewCount, Text),
      loop();

    get_count ->
      getCounter(counter),
      loop();

    {get_count_sync, From} ->
      Count = getCounter(counter),
      From ! {count, Count},
      loop();

    stop ->
      getCounter(counter),
      ok
  end.

getCounter(Key) ->
  Count = case get(Key) of
    undefined -> 0;
    Value -> Value
  end,
  io:format("Current count = ~p~n", [Count]),
  Count.

putCounter(Key, Count, Text) ->
  io:format("Received (~p): ~p~n", [Count, Text]),
  put(Key, Count).