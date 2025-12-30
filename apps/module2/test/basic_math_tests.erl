%%%-------------------------------------------------------------------
%%% @author hackus
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Nov 2025 12:21 AM
%%%-------------------------------------------------------------------
-module(basic_math_tests).

%% To run tests
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------
%% add/2
%% ------------------------------------------------------------
add_test() ->
  ?assertEqual(15, basic_math:add(10, 5)),
  ?assertEqual(-1, basic_math:add(-2, 1)).

%% ------------------------------------------------------------
%% sub/2
%% ------------------------------------------------------------
sub_test() ->
  ?assertEqual(5, basic_math:sub(10, 5)),
  ?assertEqual(-3, basic_math:sub(2, 5)).

%% ------------------------------------------------------------
%% safe_div/2
%% ------------------------------------------------------------
safe_div_test() ->
  ?assertEqual(5.0, basic_math:safe_div(10, 2)),
  ?assertEqual({error, division_by_zero}, basic_math:safe_div(10, 0)).
