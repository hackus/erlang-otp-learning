%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2025 8:00 PM
%%%-------------------------------------------------------------------
-module('RecursionInErlang_tests').
-author("hackus").

%% EUnit auto-includes
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------
%% Tests for sum/1 (tail-recursive) and badSum/1 (non-tail-recursive)
%% ------------------------------------------------------------

sum_empty_list_test() ->
  ?assertEqual(0, 'RecursionInErlang':sum([])).

sum_single_element_test() ->
  ?assertEqual(5, 'RecursionInErlang':sum([5])).

sum_multiple_elements_test() ->
  ?assertEqual(15, 'RecursionInErlang':sum([1,2,3,4,5])).

sum_negative_numbers_test() ->
  ?assertEqual(-6, 'RecursionInErlang':sum([-1,-2,-3])).

sum_mixed_numbers_test() ->
  ?assertEqual(3, 'RecursionInErlang':sum([-5, 10, -2])).


%% ------------------------------------------------------------
%% badSum tests — same behavior, but not tail-recursive
%% ------------------------------------------------------------

badsum_empty_list_test() ->
  ?assertEqual(0, 'RecursionInErlang':badSum([])).

badsum_single_element_test() ->
  ?assertEqual(5, 'RecursionInErlang':badSum([5])).

badsum_multiple_elements_test() ->
  ?assertEqual(15, 'RecursionInErlang':badSum([1,2,3,4,5])).

badsum_large_list_test() ->
  %% badSum is not tail-recursive — recursion depth can blow stack
  %% but 1000 is safe for tests
  List = lists:seq(1, 1000),
  ?assertEqual((1000*1001) div 2, 'RecursionInErlang':badSum(List)).
