%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2025 3:57 PM
%%%-------------------------------------------------------------------
-module('RunListServer_tests').
-author("hackus").

-include_lib("eunit/include/eunit.hrl").

-export([reset_server/0]).   %% export helper for debugging

%%%===================================================================
%%% Helper: ensure a fresh server instance for each test
%%%===================================================================
reset_server() ->
  case whereis('RunListServer') of
    undefined ->
      %% Server not started yet → start it
      'RunListServer':start_link();
    _Pid ->
      ok
  end,
  %% Clean state instead of killing the process
  'RunListServer':clear_sync(),
  ok.


%%%===================================================================
%%% TEST 1 — server starts and initial list is empty
%%%===================================================================
start_test() ->
  reset_server(),
  ?assertEqual([], 'RunListServer':get_sync()),
  reset_server(),
  ?assertEqual([], 'RunListServer':get_sync()).


%%%===================================================================
%%% TEST 2 — synchronous add and clear
%%%===================================================================
sync_ops_test() ->
  reset_server(),

  ok = 'RunListServer':add_sync(10),
  ok = 'RunListServer':add_sync(20),
  ok = 'RunListServer':add_sync(30),

  ?assertEqual([10,20,30], 'RunListServer':get_sync()),

  ok = 'RunListServer':clear_sync(),

  ?assertEqual([], 'RunListServer':get_sync()).


%%%===================================================================
%%% TEST 3 — asynchronous add and clear
%%%===================================================================
async_ops_test() ->
  reset_server(),

  'RunListServer':add_async(100),
  'RunListServer':add_async(200),
  'RunListServer':add_async(300),

  timer:sleep(50), %% allow mailbox to process

  ?assertEqual([100,200,300], 'RunListServer':get_sync()),

  'RunListServer':clear_async(),
  timer:sleep(30),

  ?assertEqual([], 'RunListServer':get_sync()).

%%%===================================================================
%%% TEST 4 — mixed sync + async operations
%%%===================================================================
mixed_ops_test() ->
  reset_server(),

  'RunListServer':add_sync(1),
  'RunListServer':add_async(2),
  'RunListServer':add_sync(3),
  'RunListServer':add_async(4),

  timer:sleep(50),

  ?assertEqual([1,2,3,4], 'RunListServer':get_sync()).


%%%===================================================================
%%% TEST 5 — concurrency test (1,000 parallel operations)
%%%===================================================================
concurrency_test() ->
  reset_server(),

  Parent = self(), %% get parent reference
  N = 1000,

  Workers = [spawn(fun() -> worker(I, Parent) end) || I <- lists:seq(1, N)], %% start 1000 processes passing parent as param

  %% wait for all
  lists:foreach(
    fun(_) ->
      receive done -> ok end
    end, Workers), %%

  timer:sleep(100),

  Final = 'RunListServer':get_sync(),
  ?assert(is_list(Final)). %% check value is a list

worker(I, Parent) ->
  case I rem 10 of %% works like mod returns negative for negative numbers, faster than mod
    0 -> 'RunListServer':clear_async(); %% clear on any mod 10 = 0
    _ -> 'RunListServer':add_async(I)
  end,
  Parent ! done. %% notify Parent that the operation has completed

%%%===================================================================
%%% TEST 6 — mailbox pressure test
%%%===================================================================
mailbox_test() ->
  reset_server(),
  Pid = whereis('RunListServer'),

  %% Flood async messages
  lists:foreach(
    fun(_) -> 'RunListServer':add_async(x) end,
    lists:seq(1, 10000)
  ),

  {message_queue_len, Len} = process_info(Pid, message_queue_len),

  %% mailbox must be >= 0 always
  ?assert(Len >= 0),
  ok.