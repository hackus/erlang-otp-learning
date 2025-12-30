%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2025 8:00 PM
%%%-------------------------------------------------------------------
-module('ListsInErlang_tests').
-author("hackus").

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------
%% Tests for push/2
%%------------------------------------------------------------

push_test() ->
  ?assertEqual([1], 'ListsInErlang':push(1, [])),
  ?assertEqual([2,1], 'ListsInErlang':push(2, [1])),
  ?assertEqual([3,2,1], 'ListsInErlang':push(3, [2,1])).

%%------------------------------------------------------------
%% Tests for reverse/1
%%------------------------------------------------------------

reverse_test() ->
  ?assertEqual([], 'ListsInErlang':reverse([])),
  ?assertEqual([1], 'ListsInErlang':reverse([1])),
  ?assertEqual([3,2,1], 'ListsInErlang':reverse([1,2,3])).

%%------------------------------------------------------------
%% Tests for handle/1 - add
%%------------------------------------------------------------

handle_add_test() ->
  {ok, Result} = 'ListsInErlang':handle([add, 10, [1,2,3]]),
  ?assertEqual([10,1,2,3], Result).

%%------------------------------------------------------------
%% Tests for handle/1 - remove
%%------------------------------------------------------------

handle_remove_test() ->
  {ok, Result} = 'ListsInErlang':handle([remove, 2, [10,2,3]]),
  ?assertEqual([10,3], Result).

%%------------------------------------------------------------
%% Tests for handle/1 - length
%%------------------------------------------------------------

handle_length_test() ->
  {ok, Len} = 'ListsInErlang':handle([length, [99,88,77]]),
  ?assertEqual(3, Len).

%%------------------------------------------------------------
%% Tests for handle/1 - stop
%%------------------------------------------------------------

handle_stop_test() ->
  {ok, stopped} = 'ListsInErlang':handle([stop]).

%%------------------------------------------------------------
%% Tests for handle/1 - unknown
%%------------------------------------------------------------

handle_unknown_test() ->
  ?assertMatch({error, _}, 'ListsInErlang':handle([xxx, 1, 2])).

%%------------------------------------------------------------
%% Exemplifies lists:map usage
%%-----------------------------------------------------------

listMapExample_test() ->
  ?assertEqual([[1,1],[2,2],[3,3]], 'ListsInErlang':listMapExample([1,2,3])),
  ?assertEqual([], 'ListsInErlang':listMapExample([])).

%%------------------------------------------------------------
%% Exemplifies lists:flatMap usage
%%-----------------------------------------------------------

listFlatMapExample_test() ->
  ?assertEqual([1,1,2,2,3,3], 'ListsInErlang':listFlatMapExample([1,2,3])),
  ?assertEqual([], 'ListsInErlang':listFlatMapExample([])).

%%------------------------------------------------------------
%% Exemplifies list comprehension
%%-----------------------------------------------------------

applyComprehension_test() ->
  ?assertEqual([1,1,2,2,3,3], 'ListsInErlang':applyComprehension([1,2,3])),
  ?assertEqual([], 'ListsInErlang':applyComprehension([])),
  %% Ensure same result as flatmap:
  ?assertEqual(
    'ListsInErlang':listFlatMapExample([4,5]),
    'ListsInErlang':applyComprehension([4,5])
  ).
